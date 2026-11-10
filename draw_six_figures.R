library(ggplot2);library(patchwork);library(jsonlite);library(grid)
source('panel_alignment.R')
out <- '.';qa <- 'qa';dir.create(qa,recursive=TRUE,showWarnings=FALSE)
d <- fromJSON(file.path(out,'figure_source_data.json'),simplifyVector=FALSE)
blue <- '#216A83';orange <- '#C27439';grey <- '#7B8791';ink <- '#243544';pale <- '#EAF2F5'
update_geom_defaults('point',list(stroke=1.1))
cols <- c('Sobolev'=grey,'Final model'=blue,'Smoothed'=grey,'Exact'=blue,'Angular'=grey,'Ratio-weighted'=blue,'Before'=grey,'After'=blue)
theme_set(theme_classic(base_size=24,base_family='Arial')+theme(
 axis.line=element_line(linewidth=1.42,colour=ink),axis.ticks=element_line(linewidth=1.42),
 axis.text=element_text(size=24,colour=ink),axis.text.x=element_text(margin=margin(t=12)),axis.text.y=element_text(margin=margin(r=12)),axis.title=element_text(size=24),strip.text=element_text(size=24),strip.background=element_rect(linewidth=1.42),
 plot.title=element_text(size=28,face='bold',colour=ink,margin=margin(b=18)),
 plot.subtitle=element_text(size=24,colour=grey,margin=margin(b=18)),
 plot.caption=element_text(size=24,hjust=0,margin=margin(t=15)),
 legend.position='bottom',legend.title=element_blank(),legend.text=element_text(size=24),
 plot.margin=margin(30,30,30,30),plot.tag=element_text(size=30,face='bold')))
blank <- function(title,sub=NULL){ggplot()+coord_cartesian(xlim=c(0,10),ylim=c(0,10),expand=FALSE)+theme_void(base_family='Arial',base_size=24)+labs(title=title,subtitle=sub)+theme(plot.title=element_text(size=28,face='bold',colour=ink,margin=margin(b=18)),plot.subtitle=element_text(size=24,colour=grey,margin=margin(b=18)),plot.margin=margin(30,30,30,30))}
txt <- function(p,x,y,label,size=24,col=ink,hjust=.5){p+annotate('text',x=x,y=y,label=label,size=size/2.845,family='Arial',colour=col,hjust=hjust,lineheight=1.15)}
box <- function(p,x,y,w,h,label,fill=pale){if(grepl('\n',label,fixed=TRUE))h<-max(h,2.4);p+annotate('rect',xmin=x-w/2,xmax=x+w/2,ymin=y-h/2,ymax=y+h/2,fill=fill,colour=blue,linewidth=1.42)+annotate('text',x=x,y=y,label=label,size=24/2.845,family='Arial',colour=ink,lineheight=1.15)}
arr <- function(p,x,y,xe,ye){p+annotate('segment',x=x,y=y,xend=xe,yend=ye,colour=blue,linewidth=1.42,arrow=arrow(length=unit(.16,'inches')))}
chart <- function(dat,title,ylabel,log=FALSE){p<-ggplot(dat,aes(x=x,y=value,colour=method,group=method))+geom_line(linewidth=1.42)+geom_point(size=4)+scale_colour_manual(values=cols)+labs(title=title,x=NULL,y=ylabel);if(log)p<-p+scale_y_log10();p}
v <- function(x)unlist(x,use.names=FALSE)
figs<-list();titles<-character();notes<-character()
assemble<-function(p,ncol,title,note,heading_inset=0){
 # Apply the label style to every child plot, including theme_void schematics.
 p<-lapply(p,function(panel)panel+theme(plot.tag=element_text(family='Arial',size=30,face='bold',colour=ink)))
 if(heading_inset>0)p<-lapply(p,function(panel)panel+theme(
  plot.title=element_text(margin=margin(b=18,l=heading_inset)),
  plot.subtitle=element_text(margin=margin(b=18,l=heading_inset))))
 wrap_plots(p,ncol=ncol)+plot_annotation(title=title,caption=note,tag_levels='a',theme=theme(plot.title=element_text(family='Arial',size=34,face='bold',colour=ink,margin=margin(20,30,15,30)),plot.caption=element_text(family='Arial',size=24,colour=grey,hjust=0,margin=margin(10,30,20,30)),plot.tag=element_text(family='Arial',size=30,face='bold',colour=ink)))
}

# Figure 1: training design and local derivative evidence.
a<-blank('Paired supervision','Training design')
a<-box(a,5,8.7,8.8,1.5,'Verified plugin states and audio')
a<-arr(a,5,7.8,5,7.1);a<-box(a,5,5.8,8.8,1.6,'Target / current audio pair\nLabel: actual parameter difference')
a<-arr(a,5,4.45,5,4.0);a<-box(a,5,2.7,8.8,1.8,'Shared-potential predictor\nFour normalized parameter updates')
b<-blank('Complementary loss terms','All terms follow the saved implementation')
for(i in 1:4)b<-box(b,5,10-i*2.15,9.3,1.65,c('Parameter difference: update magnitude','Exact cosine: update direction','Local derivative: response consistency','Coarse ratio: doubled MSE contribution')[i],if(i%%2)pale else '#F4EFE8')
cdata<-data.frame(stage=factor(c('Updates 1–4000','Updates 4001–8000'),levels=c('Updates 4001–8000','Updates 1–4000')),coarse=c(48,24),fine=c(0,24))
cd<-rbind(data.frame(stage=cdata$stage,type='Coarse',value=cdata$coarse),data.frame(stage=cdata$stage,type='Fine',value=cdata$fine))
c<-ggplot(cd,aes(x=stage,y=value,fill=type))+geom_col(width=.55)+coord_flip()+scale_fill_manual(values=c(Coarse=blue,Fine=orange))+labs(title='Coarse-to-fine curriculum',subtitle='Batch size 48; fresh 8000-update training',x=NULL,y='Pairs per training batch',caption='Local derivative loss is active in stage 2.')+scale_y_continuous(breaks=c(0,24,48),expand=expansion(mult=c(0,.08)))
dd<-data.frame(x=factor(c('Before','After'),levels=c('Before','After')),value=c(d$documented$derivative_before,d$derivative$identity_mse),method=c('Before','After'))
e<-ggplot(dd,aes(x=x,y=value,colour=method))+geom_point(size=7)+scale_colour_manual(values=cols)+scale_y_log10(limits=c(1e-5,1))+labs(title='Local derivative consistency',subtitle='Mean squared deviation from identity',x=NULL,y='Derivative identity MSE (log scale)',caption='Two training centers; aggregate values only.')+theme(legend.position='none')
figs[[1]]<-assemble(list(a,b,c,e),2,'Geometry-constrained training','METHOD 1 / 3   •   Design panels a–c; saved local measurement in d. No global convergence claim.')

# Figure 2: actual loss formula and archived mathematical diagnostics.
a<-blank('Direction and magnitude','Analytical illustration, not measured data')
a<-arr(a,1,1.3,8.2,1.3);a<-arr(a,1,1.3,6.1,6.3);a<-arr(a,1,1.3,3.8,4.05)
a<-txt(a,6.4,8.3,'Correct direction\nwrong magnitude');a<-txt(a,5,0.35,'Parameter axis 1');a<-txt(a,3,5.2,'Target update')
r<-seq(.002,.10,length.out=400);df<-rbind(data.frame(x=r,value=1-r/sqrt(r*r+.01^2),method='Smoothed'),data.frame(x=r,value=0,method='Exact'))
b<-ggplot(df,aes(x=x,y=value,colour=method))+geom_line(linewidth=1.42)+scale_colour_manual(values=cols)+labs(title='Aligned-vector loss',y='Directional loss',x='Predicted update norm',subtitle='Exact alignment; smoothing scale = 0.01')
ex<-d$math$examples;df<-do.call(rbind,lapply(ex,function(z)rbind(data.frame(x=z$radius,value=z$old_radial,method='Smoothed'),data.frame(x=z$radius,value=z$corrected_radial,method='Exact'))))
c<-chart(df,'Radial-gradient check','Derivative along radial scaling')+labs(x='Predicted update norm',subtitle='Five archived analytical check points')
fc<-d$angular$comparison[['fine-train']];fd<-data.frame(x=rep(c('Direction\ncosine','Update / label\nnorm'),2),value=c(fc$control$cosine_mean,fc$control$norm_ratio_mean,fc$candidate$cosine_mean,fc$candidate$norm_ratio_mean),method=rep(c('Sobolev','Exact'),each=2))
e<-ggplot(fd,aes(x=x,y=value,colour=method))+geom_hline(yintercept=1,linewidth=1.42,linetype='dashed',colour='#CAD2D6')+geom_point(position=position_dodge(width=.35),size=6)+scale_colour_manual(values=cols)+labs(title='Saved fine-task behavior',subtitle='144 directed training pairs; mean values',x=NULL,y='Dimensionless value')+scale_y_continuous(limits=c(.95,1.03),breaks=c(.96,.98,1,1.02))
figs[[2]]<-assemble(list(a,b,c,e),2,'Scale-invariant directional supervision','METHOD 2 / 3   •   Formula-based diagnostics and archived training-pair summaries; no new training.')

# Figure 3: complete tradeoff of ratio weighting.
rc<-d$ratio$comparison;axisnames<-c('Thresh.','Ratio','Attack','Release')
df<-data.frame(x=factor(axisnames,levels=axisnames),value=c(0,44,0,0))
a<-ggplot(df,aes(x=x,y=value))+geom_col(fill=blue,width=.6)+scale_y_continuous(limits=c(0,50),breaks=c(0,25,50))+labs(title='Residual grid failures',subtitle='After exact-angle training',x=NULL,y='Failures above 0.02',caption='76 tasks; all 44 failures involve ratio.')
b<-blank('Targeted supervision','Coarse pairs only')
b<-box(b,5,7.4,8.9,2,'Threshold  ×1     Ratio  ×2\nAttack  ×1     Release  ×1')
b<-box(b,5,3.5,8.9,2,'Fine-pair contributions unchanged\nFull-batch denominator retained',fill='#F4EFE8')
g<-rc[['grid-development']];df<-data.frame(x=c('Angular','Ratio-weighted'),value=c(g$angular$success02,g$candidate$success02),method=c('Angular','Ratio-weighted'))
c<-ggplot(df,aes(x=x,y=value,fill=method))+geom_col(width=.55)+scale_fill_manual(values=cols)+labs(title='Grid recovery improves',subtitle='All four errors ≤ 0.02',x=NULL,y='Successful tasks / 76')+scale_y_continuous(limits=c(0,76),breaks=c(0,38,76))+theme(legend.position='none')
df<-data.frame(x=rep(c('Ratio\nunchanged','Ratio\nchanged'),2),value=c(v(d$documented$ratio_unchanged_mae)[1],v(d$documented$ratio_changed_mae)[1],v(d$documented$ratio_unchanged_mae)[2],v(d$documented$ratio_changed_mae)[2]),method=rep(c('Angular','Ratio-weighted'),each=2))
e<-ggplot(df,aes(x=x,y=value,fill=method))+geom_col(position='dodge',width=.6)+scale_fill_manual(values=cols)+labs(title='Coarse-task decomposition',subtitle='Mean execution error in recorded classes',x=NULL,y='Parameter MAE')
keys<-c('train','original-development','second-development','grid-development','fine-train');names<-c('Coarse','Dev. 1','Dev. 2','Grid','Fine')
df<-data.frame(x=factor(names,levels=rev(names)),value=sapply(keys,function(k)100*(rc[[k]]$candidate$mae/rc[[k]]$angular$mae-1)))
f<-ggplot(df,aes(x=x,y=value,fill=value<0))+geom_hline(yintercept=0,linewidth=1.42,colour=grey)+geom_col(width=.6)+coord_flip()+scale_fill_manual(values=c('TRUE'=blue,'FALSE'=orange),guide='none')+labs(title='Benefits and tradeoffs',subtitle='Ratio-weighted relative to Angular',x=NULL,y='Change in MAE (%)',caption='Negative = lower error; all groups retained.')
g<-rc[['fine-train']];df<-data.frame(x=c('Angular','Ratio-weighted'),value=c(g$angular$mae,g$candidate$mae),method=c('Angular','Ratio-weighted'))
h<-ggplot(df,aes(x=x,y=value,fill=method))+geom_col(width=.55)+scale_fill_manual(values=cols)+labs(title='Fine accuracy is retained',subtitle='Both models: 144/144 below 0.005',x=NULL,y='Fine-pair MAE',caption='Mean error increases slightly after weighting.')+theme(legend.position='none')
figs[[3]]<-assemble(list(a,b,c,e,f,h),3,'Axis-aware supervision for coupled parameters','METHOD 3 / 3   •   Archived pair-level aggregates share endpoints; they are not independent replicates.')

# Figure 4: implementation-grounded architecture.
a<-blank('Identical feature pathways','Real audio input: stereo, 48 kHz, 3 s')
for(i in 1:3)a<-box(a,5,c(8.2,5,1.8)[i],9,1.8,c('Target audio       Current audio','STFT: FFT 1024, hop 512\nlog(1 + 100 × magnitude)','Adaptive pooling: 128 × 128\nTraining-only mean and standard deviation')[i])
a<-arr(a,5,7.2,5,6.3);a<-arr(a,5,3.7,5,3.1)
b<-blank('Shared encoder and head','47,796 trainable parameters')
labs<-c('Conv: 2 → 16, 5 × 5, stride 2','Conv: 16 → 32, 3 × 3, stride 2','Conv: 32 → 32, 3 × 3, stride 2','Average pool 4 → flatten 512','Linear 512 → 64 → 4')
for(i in 1:5)b<-box(b,5,10.7-i*1.7,9.5,1.3,labs[i]);b<-txt(b,5,.5,'GELU after convolutions and first linear layer',24)
c<-blank('Potential difference','The same mapping is used twice')
c<-box(c,2.5,8,4.1,1.7,'Target features');c<-box(c,7.5,8,4.1,1.7,'Current features');c<-arr(c,2.5,7,2.5,5.9);c<-arr(c,7.5,7,7.5,5.9)
c<-box(c,2.5,4.8,4.1,1.7,'Shared mapping φ');c<-box(c,7.5,4.8,4.1,1.7,'Shared mapping φ');c<-arr(c,2.5,3.8,4.4,2.5);c<-arr(c,7.5,3.8,5.6,2.5);c<-box(c,5,1.4,9,1.5,'Raw update = φ(target) − φ(current)')
e<-blank('From prediction to command','Known current parameters are added outside the network')
e<-box(e,5,8.5,9,1.7,'Raw four-parameter update');e<-arr(e,5,7.5,5,6.7);e<-box(e,5,5.6,9,1.7,'Clip each update to [−0.3, 0.3]');e<-arr(e,5,4.6,5,3.8);e<-box(e,5,2.5,9,2,'Add actual current parameters\nClip command to [0, 1]')
figs[[4]]<-assemble(list(a,b,c,e),2,'Shared-potential difference architecture','STRUCTURE 1 / 2   •   Implementation schematic. Latent potentials are not asserted to be absolute true parameters.',heading_inset=38)

# Figure 5: algebraic properties, explicitly not an empirical ablation.
a<-blank('Zero update','Identical input features')
a<-box(a,5,7.6,9,2,'φ(x) − φ(x) = 0');a<-txt(a,5,4.5,'No learned stopping threshold\nis needed for identical inputs',28);a<-txt(a,5,1.8,'Code also guards exactly equal raw audio.',24)
b<-blank('Antisymmetry','Swap the target and current')
b<-box(b,5,7.6,9,2,'Δ(a, b) = φ(a) − φ(b)');b<-box(b,5,4.4,9,2,'Δ(b, a) = −Δ(a, b)',fill='#F4EFE8');b<-txt(b,5,1.3,'A structural relation, not a fitted penalty.',24)
c<-blank('Path additivity','Algebraic identity for unbounded predictions')
c<-box(c,5,7.7,9.5,2,'Δ(a, b) + Δ(b, c) = Δ(a, c)');c<-txt(c,5,4.6,'The intermediate potential cancels:\n−φ(b) + φ(b) = 0',28);c<-txt(c,5,1.6,'The same mapping and preprocessing\nmust be used for all three inputs.',24)
e<-blank('Scope of the guarantees','Consistency does not establish accuracy')
e<-box(e,5,8,9.2,1.7,'Finite precision permits rounding residuals')
e<-box(e,5,5.4,9.2,1.7,'Clipping can break executed-path additivity')
e<-box(e,5,2.5,9.2,2,'Indistinguishable audio can still map\nto different true parameter states',fill='#F4EFE8')
figs[[5]]<-assemble(list(a,b,c,e),2,'Structural consistency of parameter updates','STRUCTURE 2 / 2   •   Mathematical properties of raw predictions; not new experimental measurements or accuracy guarantees.',heading_inset=38)

# Figure 6: archived physical closed-loop evaluation with task-level endpoints.
models<-c(sobolev='Sobolev',ratio_weight='Final model');loops<-list(d$loop1$comparison,d$loop2$comparison)
trace<-function(seed){df<-do.call(rbind,lapply(names(models),function(m)data.frame(x=0:4,value=v(loops[[seed]][[m]]$mae_by_step),method=models[m])));chart(df,paste('Group',seed,'recovery trajectory'),'Parameter MAE (log scale)',TRUE)+labs(x='Actual feedback step',subtitle='Eight tasks; mean across tasks')+scale_x_continuous(breaks=0:4)}
a<-trace(1);b<-trace(2)
df<-do.call(rbind,lapply(1:2,function(s)do.call(rbind,lapply(names(models),function(m)data.frame(group=paste('G',s,sep=''),method=models[m],task=0:7,value=v(loops[[s]][[m]]$final_mae_by_task))))))
c<-ggplot(df,aes(x=method,y=value,group=task))+geom_line(colour='#C4CCD1',linewidth=1.42)+geom_point(aes(colour=method),size=4)+facet_wrap(~group)+scale_colour_manual(values=cols)+scale_x_discrete(labels=c('Final model'='Final','Sobolev'='Base'))+scale_y_log10()+labs(title='Paired final errors',subtitle='MAE reduction: G1 18.39×; G2 10.95×',x=NULL,y='Task MAE (log scale)')+theme(legend.position='none')
df<-do.call(rbind,lapply(1:2,function(s)do.call(rbind,lapply(names(models),function(m)data.frame(group=paste('G',s,sep=''),method=models[m],x=c('0.02','0.005'),value=c(tail(v(loops[[s]][[m]]$success02_by_step),1),tail(v(loops[[s]][[m]]$success005_by_step),1)))))))
e<-ggplot(df,aes(x=x,y=value,fill=method))+geom_col(position='dodge',width=.6)+facet_wrap(~group)+scale_fill_manual(values=cols)+scale_y_continuous(breaks=c(0,4,8),limits=c(0,8))+labs(title='All-axis success',subtitle='Fixed step 4; all tasks retained',x='Maximum-axis error threshold',y='Successful tasks / 8')
df<-do.call(rbind,lapply(1:2,function(s)do.call(rbind,lapply(names(models),function(m)data.frame(group=paste('G',s,sep=''),method=models[m],x=factor(axisnames,levels=axisnames),value=v(tail(loops[[s]][[m]]$axis_mae_by_step,1)[[1]]))))))
f<-ggplot(df,aes(x=x,y=value,colour=method,group=method))+geom_line(linewidth=1.42)+geom_point(size=4)+facet_wrap(~group)+scale_colour_manual(values=cols)+scale_x_discrete(labels=c('Thresh.'='T','Ratio'='R','Attack'='A','Release'='Rel'))+scale_y_log10()+labs(title='Residual error by parameter',subtitle='Mean at the fixed fourth step',x=NULL,y='Axis MAE (log scale)')
df<-do.call(rbind,lapply(1:2,function(s)data.frame(x=0:7,value=v(loops[[s]]$ratio_weight$final_max_by_task),group=paste('G',s,sep=''))))
h<-ggplot(df,aes(x=x,y=value,colour=group,shape=group))+geom_hline(yintercept=c(.005,.02),colour=c('#A9B6BE',orange),linewidth=1.42,linetype='dashed')+geom_point(size=5)+scale_colour_manual(values=c(G1=blue,G2=orange))+scale_y_log10()+scale_x_continuous(breaks=0:7)+labs(title='Retained failure boundary',subtitle='Final model; dashed limits 0.005 / 0.02',x='Task ID (zero-based)',y='Maximum-axis error (log scale)')
figs[[6]]<-assemble(list(a,b,c,e,f,h),3,'Fixed-step recovery in the real compressor','PERFORMANCE   •   Two previously inspected engineering groups, n = 8 each; fixed four-step inference. No formal held-out claim.')

# Export, measured layout gate, and all-R overview.
normalize_grob<-function(p,nc){g<-patchwork::patchworkGrob(p);for(j in 1:15){idx<-1+j+15*(0:(nc-1));mx<-do.call(grid::unit.pmax,lapply(idx,function(k)g$widths[k]));for(k in idx)g$widths[k]<-mx};g}
render<-function(g){grid::grid.newpage();grid::grid.draw(g)}
grobs<-list()
for(i in 1:6){
 stem<-file.path(out,sprintf('Figure-%02d',i));w<-if(i%in%c(3,6))30 else 24;hh<-18
 grobs[[i]]<-normalize_grob(figs[[i]],if(i%in%c(3,6))3 else 2)
 require_patchwork_panel_alignment(grobs[[i]],paste0(qa,'/fig',i,'-layout.json'),paste0(qa,'/fig',i,'-alignment.json'),w,hh,audit_script='audit_panel_alignment.py',python=Sys.getenv('FXCOMP_PYTHON',Sys.which('python')),strict=TRUE)
 grDevices::cairo_pdf(paste0(stem,'.pdf'),width=w,height=hh,family='Arial');render(grobs[[i]]);dev.off()
 svglite::svglite(paste0(stem,'.svg'),width=w,height=hh);render(grobs[[i]]);dev.off()
 ragg::agg_png(paste0(stem,'.png'),width=w,height=hh,units='in',res=300);render(grobs[[i]]);dev.off()
 cat('EXPORTED',i,'\n')
}
grDevices::cairo_pdf(file.path(out,'Six-figures.pdf'),width=30,height=18,family='Arial');for(p in grobs)render(p);dev.off()
saveRDS(figs,file='qa/figure-objects.rds')
invisible(NULL)
