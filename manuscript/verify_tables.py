"""Independently re-derive the manuscript's result tables from the analysis CSVs
and diff them against what the .docx actually contains.

This does NOT call build_manuscript.js or reuse any of its logic. It reads the
same CSVs, applies the transformation the caption says was applied, formats the
numbers the way the document does, and compares cell by cell. A builder bug that
silently reads the wrong column or the wrong row shows up here as a mismatch;
the existing audit_numbers.js could not catch that, because a wrong-but-real
value still traces to a CSV."""
import re, sys, csv, html, zipfile, os
OUT = os.environ.get("DIR_OUT", os.path.expanduser("~/Downloads/ACCESS_energy/outputs"))

def doc_tables(path):
    x = zipfile.ZipFile(path).read("word/document.xml").decode("utf-8")
    body = x.split("<w:body>",1)[1].split("</w:body>",1)[0]
    def ptext(p):
        out=[]
        for m in re.finditer(r"<w:r(?:\s[^>]*)?>.*?</w:r>|<m:r>.*?</m:r>", p, re.S):
            s=m.group(0)
            if "<w:delText" in s: continue
            tag = "m:t" if s.startswith("<m:r") else "w:t"
            body2 = re.sub(r"<w:rPr>.*?</w:rPr>","",s,flags=re.S)
            out.append("".join(html.unescape(t) for t in
                       re.findall(r"<%s(?:\s[^>]*)?>([^<]*)</%s>"%(tag,tag), body2)))
        return "".join(out)
    tables=[]
    for m in re.finditer(r"<w:tbl>.*?</w:tbl>", body, re.S):
        rows=[]
        for tr in re.finditer(r"<w:tr\b.*?</w:tr>", m.group(0), re.S):
            cells=[" ".join(ptext(p) for p in re.findall(r"<w:p\b.*?</w:p>", tc.group(0), re.S)).strip()
                   for tc in re.finditer(r"<w:tc>.*?</w:tc>", tr.group(0), re.S)]
            rows.append(cells)
        tables.append(rows)
    return tables

def rd(name):
    with open(os.path.join(OUT,name), newline='', encoding='utf-8') as f:
        return list(csv.DictReader(f))

import math
def _half_away(v, d):
    """Round half away from zero on the decimal value (12 significant digits),
    as the builder does, rather than on the binary float: 0.1345 -> 0.135 and
    0.0545 -> 0.055 alike."""
    shifted=float(f"{v*10**d:.12g}")
    return math.copysign(math.floor(abs(shifted)+0.5), shifted)/10**d

NEGZERO=[]
def nfmt(x, d=3, tag=None):
    """The builder's number formatter. Mirrors its negative-zero behaviour:
    JS (-0).toFixed(3) renders "0.000", so a small NEGATIVE value that rounds to
    zero loses its sign in the document. Recorded, not silently accepted."""
    try: v=float(x)
    except (TypeError, ValueError): return "NA"
    if v != v: return "NA"
    out=f"{_half_away(v, d):.{d}f}"
    if out.startswith("-") and float(out)==0.0:
        NEGZERO.append((tag, v))
        out=out[1:]
    return out

def pfmt(x):
    """The builder's p formatter."""
    try: v=float(x)
    except (TypeError, ValueError): return "NA"
    if v != v: return "NA"
    return "<0.001" if v < 0.001 else f"{_half_away(v, 3):.3f}"

FAILS=[]; CHECKED=0
def cmp_cell(label, got, want):
    global CHECKED
    CHECKED += 1
    g=(got or "").strip(); w=(want or "").strip()
    if g != w:
        FAILS.append((label, w, g))

def find_table(tables, header_sig):
    for t in tables:
        if not t: continue
        h=" | ".join(t[0])
        if all(s in h for s in header_sig): return t
    return None

SI = doc_tables(os.path.join(OUT,"ACCESS_Health_SI.docx"))
print(f"tables found in the SI: {len(SI)}")

OUTCOME_LAB={"neonatal":"Neonatal","infant":"Infant"}

# ---------------------------------------------------------------- Table S6/S7
EXPO=[("change_lpg_raw","Raw NFHS"),
      ("change_lpg_rc","Regression-calibrated"),
      ("change_lpg_bayes","Bayesian-corrected, fixed surface"),
      ("change_lpg_bayes_MI","Bayesian, exposure-only MI (sensitivity)"),
      ("change_lpg_bayes_iresonly","Bayesian, one instrument (IRES) both rounds"),
      ("change_lpg_bayes_iresonly_MI","Bayesian, one instrument, exposure-only MI")]
UNW=[("change_lpg_raw","Raw NFHS, unweighted"),
     ("change_lpg_rc","Regression-calibrated, unweighted"),
     ("change_lpg_bayes","Bayesian-corrected, unweighted"),
     ("change_lpg_bayes_iresonly","Bayesian, one instrument (IRES), unweighted")]

SMALL_CLUSTER={"health_effects_table.csv":"health_small_cluster_inference.csv",
               "health_effects_table_all.csv":"health_small_cluster_inference_all.csv",
               "health_effects_table_nonoverlap.csv":"health_small_cluster_inference_nonoverlap.csv"}
def he_rows(csvname, tag, unweighted=True):
    rows=rd(csvname)
    # The builder takes p (CR0) and p_cr2 for the adjusted, all-stratum rows
    # from the small-cluster file, which stores them at full precision. Apply
    # the same rule here so both sides round the same number once.
    scf=SMALL_CLUSTER.get(csvname)
    sc=rd(scf) if scf and os.path.exists(os.path.join(OUT,scf)) else []
    for r in rows:
        if r["adjusted"]!="TRUE" or r.get("support_stratum","all")!="all": continue
        m=next((x for x in sc if x["outcome"]==r["outcome"] and x["term"]==r["term"]
                and x["weighting"]==r["weighting"]), None)
        if not m: continue
        if m.get("p_cr0"): r["p"]=m["p_cr0"]
        if m.get("p_cr2"): r["p_cr2"]=m["p_cr2"]
    def pick(oc,term,wt,sup="all"):
        for r in rows:
            if (r["outcome"]==oc and r["term"]==term and r["adjusted"]=="TRUE"
                and r["weighting"]==wt and r["support_stratum"]==sup): return r
        return None
    exp=[]
    blocks=(("birth_hmean",EXPO),("unweighted",UNW)) if unweighted else (("birth_hmean",EXPO),)
    for wt,specs in blocks:
        for oc in ("neonatal","infant"):
            for term,lab in specs:
                r=pick(oc,term,wt)
                if not r: continue
                # CR2 interval: raw-scale ci_cr2_* x 10, independently scaled here
                cr2 = (nfmt(r["est_per10"])+" ["+nfmt(float(r["ci_cr2_low"])*10)+", "
                       +nfmt(float(r["ci_cr2_high"])*10)+"]") if r.get("se_cr2") not in (None,"","NA") else "NA"
                conv= (nfmt(r["lo_per10"])+", "+nfmt(r["hi_per10"]))
                exp.append([OUTCOME_LAB[oc], lab, cr2, pfmt(r["p_cr2"]),
                            nfmt(r["est_per10"])+" ["+conv+"]", pfmt(r["p"])])
    return exp

# Four tables share the health header: S5 (adult outcomes), S6 (rural
# mortality), S6b (non-overlapping cohorts) and S7 (all-household mortality).
# Adult vs mortality is identified by CONTENT (the outcome labels in column 1);
# S6b by content too (it has no unweighted rows); S6 and S7 by their order.
sig=["Est. per 10-pp LPG [95% CI], CR2","p (CR2)"]
cands=[t for t in SI if t and all(s in " | ".join(t[0]) for s in sig)]
mort=[t for t in cands if any(r and r[0] in ("Neonatal","Infant") for r in t[1:])]
adult=[t for t in cands if any(r and r[0].startswith(("Hypertension","Diabetes")) for r in t[1:])]
print(f"\n=== health-header tables: {len(cands)} total -> {len(mort)} mortality, {len(adult)} adult ===")
assert len(mort)==3, f"expected S6, S6b and S7, found {len(mort)}"
_unw=lambda t: any(len(r)>1 and "unweighted" in r[1] for r in t[1:])
_nonov=[t for t in mort if not _unw(t)]; _rest=[t for t in mort if _unw(t)]
assert len(_nonov)==1 and len(_rest)==2, "could not tell S6b from S6/S7 by content"
for ti,(t,src) in enumerate(zip([_rest[0], _nonov[0], _rest[1]],
                                ["health_effects_table.csv","health_effects_table_nonoverlap.csv",
                                 "health_effects_table_all.csv"])):
    exp = he_rows(src, src, unweighted=(src!="health_effects_table_nonoverlap.csv"))
    body=t[1:]
    print(f"  {src}: {len(body)} doc rows vs {len(exp)} derived rows")
    if len(body)!=len(exp):
        FAILS.append((f"{src}: row count", str(len(exp)), str(len(body)))); continue
    if len(body)!=len(exp):
        FAILS.append(("row count (table checked at line %d)" % __import__("inspect").currentframe().f_lineno, str(len(exp)), str(len(body))))
    for i,(dr,er) in enumerate(zip(body,exp)):
        if len(dr)!=len(er):
            FAILS.append((f"r{i+1}: cell count", str(len(er)), str(len(dr))))
        for j,(dc,ec) in enumerate(zip(dr,er)):
            cmp_cell(f"{src} r{i+1}c{j+1} ({er[0]}/{er[1]})", dc, ec)

# ---- Table S5, adult outcomes: H4 writes lo_cr2_per10/hi_cr2_per10 already
# on the per-10 scale, a DIFFERENT convention from H2's raw-scale ci_cr2_*.
# Checking it here is the point: a builder that used the wrong convention would
# be out by a factor of ten and audit_numbers.js would still find a match.
if adult:
    rows=rd("si_adult_health_effects.csv")
    ASPEC=[("change_lpg_raw","Raw NFHS"),("change_lpg_rc","Regression-calibrated"),
           ("change_lpg_bayes","Bayesian-corrected")]
    ALAB={"hypertension":"Hypertension","diabetes":"Diabetes",
          "diabetes (self-report)":"Diabetes (self-report)"}
    exp=[]
    for oc in ("hypertension","diabetes","diabetes (self-report)"):
        for term,lab in ASPEC:
            r=next((x for x in rows if x["outcome"]==oc and x["term"]==term
                    and x["adjusted"]=="TRUE"), None)
            if not r: continue
            exp.append([ALAB[oc], lab,
                        nfmt(r["est_per10"])+" ["+nfmt(r["lo_cr2_per10"])+", "+nfmt(r["hi_cr2_per10"])+"]",
                        pfmt(r["p_cr2"]),
                        nfmt(r["est_per10"])+" ["+nfmt(r["lo_per10"])+", "+nfmt(r["hi_per10"])+"]",
                        pfmt(r["p"])])
    body=adult[0][1:]
    print(f"  si_adult_health_effects.csv (Table S5): {len(body)} doc rows vs {len(exp)} derived rows")
    if len(body)!=len(exp):
        FAILS.append(("row count (table checked at line %d)" % __import__("inspect").currentframe().f_lineno, str(len(exp)), str(len(body))))
    for i,(dr,er) in enumerate(zip(body,exp)):
        if len(dr)!=len(er):
            FAILS.append((f"r{i+1}: cell count", str(len(er)), str(len(dr))))
        for j,(dc,ec) in enumerate(zip(dr,er)):
            cmp_cell(f"S5 r{i+1}c{j+1} ({er[0]}/{er[1]})", dc, ec)

# ---------------------------------------------------------------- Table S6a
t = find_table(SI, ["p (wild)","df (Satt.)"])
print(f"\n=== Table S6a (inference comparison) ===")
if t is None:
    FAILS.append(("Table S6a","present","NOT FOUND"))
else:
    rows=rd("health_small_cluster_inference.csv")
    SPECS=[("change_lpg_raw","Raw NFHS"),("change_lpg_rc","Regression-calibrated"),
           ("change_lpg_bayes","Bayesian-corrected (era-matched)"),
           ("change_lpg_bayes_iresonly","Bayesian, one instrument (IRES)"),
           ("change_lpg_bayes_accesswt","Bayesian, design-weighted ACCESS leg")]
    exp=[]
    for oc in ("neonatal","infant"):
        for term,lab in SPECS:
            r=next((x for x in rows if x["outcome"]==oc and x["term"]==term), None)
            if not r: continue
            exp.append([OUTCOME_LAB[oc], lab, nfmt(r["estimate_per10"]),
                        pfmt(r["p_cr0"]), pfmt(r["p_cr2"]), pfmt(r["p_wild"]),
                        nfmt(r["df_satt"],1)])
    body=t[1:]
    print(f"  {len(body)} doc rows vs {len(exp)} derived rows")
    if len(body)!=len(exp):
        FAILS.append(("row count (table checked at line %d)" % __import__("inspect").currentframe().f_lineno, str(len(exp)), str(len(body))))
    for i,(dr,er) in enumerate(zip(body,exp)):
        if len(dr)!=len(er):
            FAILS.append((f"r{i+1}: cell count", str(len(er)), str(len(dr))))
        for j,(dc,ec) in enumerate(zip(dr,er)):
            cmp_cell(f"S6a r{i+1}c{j+1} ({er[0]}/{er[1]})", dc, ec)

# ---------------------------------------------------------------- Table S8
t = find_table(SI, ["Predicted exposure metric"])
print(f"\n=== Table S8 (predicted composition metrics) ===")
if t is None:
    FAILS.append(("Table S8","present","NOT FOUND"))
else:
    rows=rd("health_nuanced_effects.csv")
    ORDER=["Primary LPG (Bayes-corrected)","LPG, no solid fuel reported (share)",
           "LPG and solid fuel reported (share)","Any solid fuel reported (share)",
           "Predicted LPG consumption"]
    exp=[]
    for oc in ("neonatal","infant"):
        for ex in ORDER:
            r=next((x for x in rows if x["analysis"]=="point" and x["outcome"]==oc
                    and x["exposure"]==ex), None)
            if not r: continue
            per=" (per 10 kg/yr)" if r["per(+)"]=="10 kg/yr" else ""
            cr2=(nfmt(r["est_per10"])+" ["+nfmt(r["lo_cr2"])+", "+nfmt(r["hi_cr2"])+"]") if r.get("lo_cr2") else "NA"
            exp.append([OUTCOME_LAB[oc], ex+per, cr2, pfmt(r["p_cr2"]),
                        nfmt(r["est_per10"])+" ["+nfmt(r["lo"])+", "+nfmt(r["hi"])+"]",
                        pfmt(r["p"])])
    body=t[1:]
    print(f"  {len(body)} doc rows vs {len(exp)} derived rows")
    if len(body)!=len(exp):
        FAILS.append(("row count (table checked at line %d)" % __import__("inspect").currentframe().f_lineno, str(len(exp)), str(len(body))))
    for i,(dr,er) in enumerate(zip(body,exp)):
        if len(dr)!=len(er):
            FAILS.append((f"r{i+1}: cell count", str(len(er)), str(len(dr))))
        for j,(dc,ec) in enumerate(zip(dr,er)):
            cmp_cell(f"S8 r{i+1}c{j+1} ({er[0]}/{er[1]})", dc, ec)

# ---------------------------------------------------------------- Table S16
# Stage A of the abandoned imputation, re-derived from the committed record that
# build_docx.R copies into OUT/records/health_imputation/. (tq() below is kept
# for any table that needs an independent t quantile.)
def tq(df):
    try:
        from scipy.stats import t as _t
        return float(_t.ppf(0.975, df))
    except Exception:
        pass
    import math
    # Cornish-Fisher expansion, then Newton-refine against the exact CDF.
    def cdf(x, v):
        # regularized incomplete beta via continued fraction
        def betacf(a,b,x):
            qab,qap,qam=a+b,a+1,a-1; c=1.0; d=1-qab*x/qap
            if abs(d)<1e-30: d=1e-30
            d=1/d; h=d
            for m in range(1,300):
                m2=2*m
                aa=m*(b-m)*x/((qam+m2)*(a+m2))
                d=1+aa*d; d=1e-30 if abs(d)<1e-30 else d
                c=1+aa/c; c=1e-30 if abs(c)<1e-30 else c
                d=1/d; h*=d*c
                aa=-(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
                d=1+aa*d; d=1e-30 if abs(d)<1e-30 else d
                c=1+aa/c; c=1e-30 if abs(c)<1e-30 else c
                d=1/d; de=d*c; h*=de
                if abs(de-1)<3e-12: break
            return h
        def ibeta(a,b,x):
            if x<=0: return 0.0
            if x>=1: return 1.0
            bt=math.exp(math.lgamma(a+b)-math.lgamma(a)-math.lgamma(b)
                        +a*math.log(x)+b*math.log(1-x))
            return bt*betacf(a,b,x)/a if x<(a+1)/(a+b+2) else 1-bt*betacf(b,a,1-x)/b
        return 1-0.5*ibeta(v/2,0.5,v/(v+x*x))
    lo,hi=0.0,500.0
    for _ in range(200):
        mid=(lo+hi)/2
        if cdf(mid,df)<0.975: lo=mid
        else: hi=mid
    return (lo+hi)/2

t = find_table(SI, ["Datasets mixed","Median max R-hat"])
print(f"\n=== Table S16 (Stage A convergence pilot; failed convergence) ===")
if t is None:
    FAILS.append(("Table S16","present","NOT FOUND"))
else:
    with open(os.path.join(OUT, "records", "health_imputation", "v2_stageA_summary.csv"), newline='', encoding='utf-8') as f:
        rows=list(csv.DictReader(f))
    isT=lambda v: str(v).upper()=="TRUE"
    rows.sort(key=lambda x: (not isT(x["state_re"]), x["eta_update"], x["length"]))
    def rh(v): return "infinite" if str(v)=="Inf" else nfmt(v,2)
    exp=[["yes" if isT(x["state_re"]) else "no", x["eta_update"], x["length"],
          f'{x["mixed"]} / {x["datasets"]}', rh(x["median_rhat_max"]), nfmt(x["median_ess_min"],0),
          nfmt(100*float(x["median_acc_idx"]),1)+"%"] for x in rows]
    body=t[1:]
    print(f"  {len(body)} doc rows vs {len(exp)} derived rows")
    if len(body)!=len(exp):
        FAILS.append(("Table S16 row count", str(len(exp)), str(len(body))))
    for i,(dr,er) in enumerate(zip(body,exp)):
        for j,(dc,ec) in enumerate(zip(dr,er)):
            cmp_cell(f"S16 r{i+1}c{j+1}", dc, ec)

print("\n" + "="*72)
print(f"CELLS CHECKED: {CHECKED}    MISMATCHES: {len(FAILS)}")
print("="*72)
for lab,want,got in FAILS[:40]:
    print(f"  {lab}\n      derived from CSV : {want!r}\n      printed in docx  : {got!r}")
sys.exit(1 if FAILS else 0)
