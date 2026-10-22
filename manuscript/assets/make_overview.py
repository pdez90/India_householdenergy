import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch
W,H=7.6,10.0
fig=plt.figure(figsize=(W,H),dpi=204); ax=fig.add_axes([0,0,1,1]); ax.set_xlim(0,W); ax.set_ylim(0,H); ax.axis("off")
BLUE="#1f5fa6"; LB="#d4e3f2"; GR="#f0f3f6"; EDGE="#9aa7b4"; INK="#1d2733"; SUB="#4a5562"
def box(x,y,w,h,title,sub,fc=GR,ec=EDGE,tc=INK,sc=SUB,lw=1.4,ts=11.5):
    ax.add_patch(FancyBboxPatch((x,y),w,h,boxstyle="round,pad=0.02,rounding_size=0.08",fc=fc,ec=ec,lw=lw))
    if sub:
        ax.text(x+w/2,y+h*0.66,title,ha="center",va="center",fontsize=ts,fontweight="bold",color=tc)
        ax.text(x+w/2,y+h*0.30,sub,ha="center",va="center",fontsize=8.6,color=sc,linespacing=1.25)
    else:
        ax.text(x+w/2,y+h/2,title,ha="center",va="center",fontsize=ts,fontweight="bold",color=tc)
def arrow(x0,y0,x1,y1,c=BLUE):
    ax.annotate("",xy=(x1,y1),xytext=(x0,y0),arrowprops=dict(arrowstyle="-|>",color=c,lw=1.6,mutation_scale=12))
box(0.3,8.75,3.3,0.95,"NFHS-4 / NFHS-5","national health survey; district resolution;\nhealth outcomes; single main-fuel item",fc=LB,ec=BLUE)
box(4.0,8.75,3.3,0.95,"ACCESS / IRES","specialized energy surveys; full household\nfuel portfolio; limited coverage; no health",fc=LB,ec=BLUE)
arrow(1.95,8.75,2.9,8.33); arrow(5.65,8.75,4.7,8.33)
box(1.6,7.55,4.4,0.78,"Common district-level estimation","multilevel and design-weighted estimates on one district frame")
arrow(3.8,7.55,3.8,7.13)
box(1.6,6.35,4.4,0.78,"Cross-survey comparison","NFHS vs energy survey, era-matched; benchmark (falsification) items")
arrow(2.7,6.35,1.95,5.93); arrow(4.9,6.35,5.65,5.93)
box(0.3,4.85,3.3,1.08,"Measurement-error calibration","regression calibration +\nBayesian measurement-error model\n-> calibrated primary-LPG surface, with uncertainty")
box(4.0,4.85,3.3,1.08,"Fuel stacking","measured in the energy surveys; where it varies;\nprediction from NFHS covariates tested\n(did not transfer reliably)")
arrow(1.95,4.85,2.9,4.43)
box(1.6,3.55,4.4,0.88,"Calibrated district exposure surface","rural primary-LPG prevalence; uncertainty split into\ncalibration-side and health-survey-side parts",fc=BLUE,ec=BLUE,tc="white",sc="#e8eef6",ts=12)
arrow(3.8,3.55,3.8,3.13)
box(1.6,2.25,4.4,0.88,"Exploratory health illustration","child mortality, district change-on-change;\nconditional on the calibration; not a validated estimate")
arrow(3.8,2.25,3.8,1.83,c=INK)
box(0.6,0.55,6.4,1.28,"General lesson","specialized surveys can detect and calibrate error in a simplified\nexposure item; constructs the item does not record, such as stacking,\nmust be measured, and calibrated surfaces should carry their uncertainty",fc="white",ec=INK,ts=12)
fig.savefig("fig_overview.png",dpi=204,metadata={"Software":None})
