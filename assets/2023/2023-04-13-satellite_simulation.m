(* ::Package:: *)

(* ::Subsubsection::Initialization:: *)
(*Set initial parameters*)


(* ::Input::Initialization:: *)
InitialHeight = 4000    ;                         (*initial height of the object from the surface of the Earth in MILES*)  
InitialSpeed  =   10400     ;                        (*enter the initial speed of the object in MILES/HOUR, fired horizontally*) 


airFriction = 0  ;                                              (*Amount of air friction; 0 indicates no air*)    
DisplayRange = {-1 10^7, 1.3 10^7};            (*Don't worry about this parameter; for dispaly purposes only*)



(* ::Subsubsection::Initialization::Closed:: *)
(*Unsightly code (hidden from public view)*)


(* ::Input::Initialization:: *)
FinalTime = 48000 ;                                      (*how long (in SECONDS) do you want the simulation to run?*)
start = 10^-5;
v0KmpHr = InitialSpeed/0.62;                               (*initial speed of the satellite in km/hr fired horizontally*) 
r0 =(InitialHeight + 3958)  1.61  10^3 ;         (*initial distance of the satellite from the centre of the Earth in metres*)
G = 6.67 10^-11 ;                                 (*gravitational constant*)
M = 5.97219*10^24 ;                            (*mass of earth in kilograms*)
v0Mps  = v0KmpHr (5/18) ;                      (*initial speed of the satellite in m/s fired horizontally*) 
phid0 = v0Mps/r0;   
RE = 6378 1000 ;                              (*radius of the Earth*)

StopCriterion =      {WhenEvent[  { r[t]  < RE || t >FinalTime}   ,end=t;"StopIntegration"] } ;     
sol=NDSolve[{r[t] phi''[t]+ 2 r'[t]phi'[t] ==0 -airFriction  phi'[t]   ,r''[t] - r[t]phi'[t]^2   == (- (G M ))/r[t]^2       ,r[0]==r0, phi[0]==\[Pi]/2 ,r'[0]==0, phi'[0]== phid0, StopCriterion},{r,phi},{t,\[Infinity]}] //Quiet    ;

p1[time_]  :=ParametricPlot[Evaluate[{r[t] Cos[phi[t]], r[t] Sin[phi[t]]}/. sol],{t,0,time}, PlotRange->DisplayRange, PlotStyle->Red, ImageSize->Large , Ticks->None, Axes->{False, False}  ]  ;
p2a=Plot[ (RE^2-x^2)^(1/2), {x,-RE,RE}, Filling->Axis];
p2b=Plot[- (RE^2-x^2)^(1/2), {x,-RE,RE}, Filling->Axis];


(* ::Subsubsection::Initialization:: *)
(*Visualize the path*)


(* ::Input::Initialization:: *)
Manipulate[  Show[p1[time],p2a,p2b]   ,  {time,start,end}  ]


(* ::Input::Initialization:: *)

