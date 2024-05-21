breed [dogs a-dog]
breed [sheep a-sheep]

globals
[
  cycle
  best-average-fitness-this-cycle
  best-average-fitness-overall
  averageFitness
  currentGeneration
  best-individual-score
  highest-avg-fitness
  average-score
  last-dog-fitnesses
  currentChrom
]

sheep-own [
  last-direction; move taken last tick
]

dogs-own [
  chromosome ; TODO - EXPLAIN CHROMOSOME
  generation ;; what generation they are from
  current-fitness;
  current-avg-fitness
]

to setup
  clear-all
  set best-average-fitness-overall 0
  set best-average-fitness-this-cycle 0
  set last-dog-fitnesses []

  ; set behaviour from the training
  let chromosomes [[1 45 1.1956739611046414] [2 9 1.5314295965089653] [2 38 1.143777241013512] [0 39 1.9540880034099213] [0 26 0.31559374853140176]]
  let n 0

  ask patches [
    set pcolor green
  ]
  create-sheep initial-number-sheep [
    setxy random-xcor random-ycor
    set color white
    set shape "sheep"
    set last-direction 0
  ]
  create-dogs initial-number-dogs [
    setxy random-xcor random-ycor
    set color red
    set shape "wolf"
    ; Set chromosome to be an array of the size of the number of dogs
    set chromosome item n chromosomes
    set n n + 1
    set generation 0
    set current-fitness 999999999999
    set current-avg-fitness 0;
  ]
  reset-ticks
end

;to setup-chromosome
;  set chromosome (list (random 4) (random 50) (random-float 2))
;end

to go
  ask dogs [
    if default-dog-movement [
      move-dogs-default 1
    ]
    move-dogs
  ]
  ask sheep [
    move-sheep
  ]

  if ticks > 0 [
    if ticks mod cycleTime = 0
    [
      endOfCycle
    ]
  ]
  tick

end

; Default random walk implementation
to move-dogs-default [movement-amount]

  let possible-moves ["left" "right" "up" "down" "stay"]
  ; move left right, up, down or stay still

  let movement one-of possible-moves

  if movement = "left" [
    set xcor max list (xcor - movement-amount) min-pxcor
  ]
  if movement = "right"[
    set xcor min list (xcor + movement-amount) max-pxcor
  ]
  if movement = "up" [
    set ycor min list (ycor + movement-amount) max-pycor
  ]
  if movement = "down" [
    set ycor max list (ycor - movement-amount) min-pycor
  ]
end

to move-dogs

  let norm-max count dogs
  let norm-min count dogs * -1

  ; Unpack chromosome
  let action-type item 0 chromosome
  let distance-condition item 1 chromosome
  let movement item 2 chromosome


  ; Role 1 - Pusher
  if action-type = 0 [
    push-action distance-condition movement
    stop
  ]

  ; Role 2 - Fetcher
  if action-type = 1 [
    fetch-action distance-condition movement
    stop
  ]

  ; Role 3 - Keep distance from center then fetch or push
  if action-type = 2 [
    let s count sheep

    ; Get the mean of x and y
    let mean-x 0
    let mean-y 0
    ask sheep [
      set mean-x mean-x + xcor
      set mean-y mean-y + ycor
    ]
    set mean-x mean-x / s
    set mean-y mean-y / s

    ifelse (distancexy mean-x mean-y) < distance-condition [
      facexy mean-x mean-y
      fd 1
    ] [
      ; We are a suitible distance away, now fetch or push
      let follow-up-action random 2
      if follow-up-action = 0 [
        push-action distance-condition movement
      ]
      if follow-up-action = 1 [
        fetch-action distance-condition movement
      ]
    ]
    stop
  ]

  ; role 4 - random walk
  ; Random walk
  if action-type = 3 [
    move-dogs-default movement
 ]



end

to fetch-action [distance-condition movement]
    let furthest-sheep max-one-of sheep [distance myself]
    let distance-furthest 0
    ask furthest-sheep [
        set distance-furthest distance myself
    ]

    if distance-furthest < distance-condition [
      face furthest-sheep
      fd movement
    ]
end

to push-action [distance-condition movement]
  let nearest-sheep min-one-of sheep [distance myself]
    let distance-nearest 0
    ask nearest-sheep [
        set distance-nearest distance myself
    ]

    if distance-nearest < distance-condition [
      face nearest-sheep
      fd movement
    ]
end

to move-sheep

 let current-patch patch-here
 let adjacent-patches neighbors4

 let adjacent-dog-free-patches adjacent-patches with [not any? dogs-here]
 let adjacent-dog-patches adjacent-patches with [any? dogs-here]


 let adjacent-sheep-free-patches adjacent-patches with [not any? other sheep-here]
 let adjacent-sheep-patches adjacent-patches with [any? other sheep-here]


 ; 1 - Move away from the dogs
 if any? dogs-here [
    if any? adjacent-dog-free-patches [
      let target-patch one-of adjacent-dog-free-patches
      face target-patch
      set last-direction heading
      fd 1
    ]
    stop
 ]


 ; 2 - If a dog is in an adjacent patch, move to one without
 if any? adjacent-dog-patches [
    let target-patch one-of adjacent-dog-free-patches
    face target-patch
    set last-direction heading
    fd 1
    stop
 ]

 ; 3 - Move to a patch with no sheep, but is adjacent to a patch with sheep
 if any? adjacent-sheep-free-patches [
    ; i.e. patches that contain no sheep but contain sheep in the adjacent four patches
    let this-sheep self
    ; Build up an array of adjacent patches to the sheep-free patches

    let candidates []
    ask adjacent-sheep-free-patches [
      let adjacent neighbors4

      ask adjacent [
        let other-sheep-at-patch sheep-here with [self != this-sheep]

        if any? other-sheep-at-patch [
          set candidates lput self candidates
        ]
      ]
    ]

    ; Convert to a patch set
    set candidates patch-set candidates
    ; a candidate is eligable is there exists a sheep that is not the agent in it
    ; Remove current patch from patch set
    ask patch-here [set candidates other candidates]

    if any? candidates[
      let target-patch one-of candidates
      face target-patch
      set last-direction heading
      fd 1
      stop
    ]
 ]

 ; 4 - Move to an adjacent patch containing fewer sheep than the current patch
 let current-sheep-on-patch count sheep-here
 let adjacent-with-less-sheep adjacent-patches with [count sheep-here < current-sheep-on-patch]
 if any? adjacent-with-less-sheep [
    let target-patch one-of adjacent-with-less-sheep
    face target-patch
    fd 1
    stop
 ]

 ; 5 - a stochastic choice of action as follows: choose the same action as the last
 ; one with 50% probability, or choose one of the remaining four actions, each with
 ; 12.5% probability. For the first move, assume for all sheep that their previous
 ; move was to stay put.

 ; Assumption - There is no point attempting to run the other actions, because if they were
 ; possible, they would have already been done due to them having a higher priority

  if random-float 1 > 0.5 [
    set heading last-direction
    fd 1
  ]


end

; Score / fitness function
to-report calculate-score
  let s count sheep

  ; Get the mean of x and y
  let mean-x 0
  let mean-y 0
  ask sheep [
    set mean-x mean-x + xcor
    set mean-y mean-y + ycor
  ]
  set mean-x mean-x / s
  set mean-y mean-y / s

  let sum-of-variation-x 0
  let sum-of-variation-y 0
  ask sheep [
    set sum-of-variation-x sum-of-variation-x + ((xcor - mean-x) ^ 2)
    set sum-of-variation-y sum-of-variation-y + ((ycor - mean-y) ^ 2)
  ]

  let score (sum-of-variation-x + sum-of-variation-y) / s
  set average-score ((average-score * (ticks - 1)) + score) / ticks
  report score
end

; Fitness function
; nearest-largest-group
to-report fitness
  let nearest-group min-one-of (patches with [count sheep-here >= 2] in-radius 8) [ distance myself ]

  let sheep-at-patch 0

  if nearest-group = nobody [
    report 0
  ]

  ask nearest-group [
     set sheep-at-patch count sheep-here
  ]

  set current-fitness sheep-at-patch

  report sheep-at-patch
end

; average distance to all sheep averaged across all dogs
to-report average-fitness
  let c count dogs
  let total 0
  ask dogs [
     set total total + fitness
  ]
  if c > 0 [
    let average total / c
    if average > best-average-fitness-this-cycle [
      set best-average-fitness-this-cycle average
    ]
    if average > best-average-fitness-overall [
      set best-average-fitness-overall average
    ]
    report average
  ]
  report 0 ; if no dogs, report 0 , avoids div by 0
end

; GA METHODS

to endOfCycle
  set best-average-fitness-this-cycle 0

  update-average-dog-fitnesses

  ; bodge to get things working for test
  ask dogs [
    set generation currentGeneration
  ]

  set currentGeneration currentGeneration + 1

  ; remove GA aspects
  ; mutate
  ;ask dogs [
;
 ;   mutate-chromosome
;
 ; ]

  ; hatch and crossover
  ;hatchNextGeneration


end

to setup-dog
  set generation currentGeneration

;  ; when a new agent is created during the tournament phase it will have a chance to mutate chromosome data
;  ifelse generation = 0
;  [
;    setup-chromosome
;  ]
;  [
;    mutate-chromosome
;  ]
end

to hatchNextGeneration

  let tempSet (dogs with [generation = currentGeneration])

  set currentGeneration (currentGeneration + 1)

  ; stops div by 0
  if averageFitness = 0 [ set averageFitness 1]

  while[ count dogs < (initial-number-dogs * 2)]
  [
    ask tempSet
    [
      if count dogs < (initial-number-dogs * 2)
      [

        if (current-fitness / averageFitness) > random-float 1
        [
          hatch-dogs 1 [setup-dog]
        ]
      ]
    ]
  ]
  ask tempSet [die]

  crossOver
end

; Mutation function
to mutate-chromosome

  let mutated-chrom []

  ; potentially apply mutation to gene 1 - action number 1-9
  ifelse random-float 1 < mutation-probability [
    set mutated-chrom lput (random 4) mutated-chrom
  ] [
    set mutated-chrom lput item 0 chromosome mutated-chrom
  ]

  ; potentially apply mutation to gene 2 - distance condition 1-50
  ifelse random-float 1 < mutation-probability [
    set mutated-chrom lput (random 50) mutated-chrom
  ] [
    set mutated-chrom lput item 1 chromosome mutated-chrom
  ]

  ; potentially apply mutation to gene 3 - distance amount -1 or 1
  ifelse random-float 1 < mutation-probability [
    set mutated-chrom lput random-float 2 mutated-chrom
  ] [
    set mutated-chrom lput item 2 chromosome mutated-chrom
  ]

  set chromosome mutated-chrom
end

; let the top two breed
; pick a point to crossover between
; a funky one point crossover
to crossOver

  if random-float 1 > crossover-probability [
    let tempSet (other dogs with [generation = currentGeneration])

    let parent1 max-one-of (n-of 3 tempSet) [fitness]
    let parent2 max-one-of (n-of 3 tempSet) [fitness]

    let crossoverPoint one-of [1 2]

    let cx-chrom-a1 []
    let cx-chrom-a2 []

    ask parent1 [
      let a1-action-type item 0 chromosome
      let a1-distance-condition item 1 chromosome
      let a1-movement item 2 chromosome


      ask parent2 [
        let a2-action-type item 0 chromosome
        let a2-distance-condition item 1 chromosome
        let a2-movement item 2 chromosome

        if crossoverPoint = 1 [
          set cx-chrom-a1 (list a1-action-type a2-distance-condition a2-movement)
          set cx-chrom-a2 (list a2-action-type a1-distance-condition a1-movement)
        ]

        if crossoverPoint = 2 [
          set cx-chrom-a1 (list a1-action-type a1-distance-condition a2-movement)
          set cx-chrom-a2 (list a2-action-type a2-distance-condition a1-movement)
        ]

        set chromosome cx-chrom-a2
      ]
      set chromosome cx-chrom-a1
    ]
  ]
end

to update-average-dog-fitnesses
  ask dogs [
    set current-avg-fitness ((current-avg-fitness * generation) + fitness) / (generation + 1)
  ]
  print "end of cycle dog-fitnesses: "
  print current-average-dog-fitnesses
end

to-report get-avg-dog-fitnesses
  if ticks mod cycleTime = 0 [
    set last-dog-fitnesses current-average-dog-fitnesses
  ]
  report last-dog-fitnesses
end

to-report current-average-dog-fitnesses
  let array []
  ask dogs [
   set array lput current-avg-fitness array
  ]
  report array
end
@#$#@#$#@
GRAPHICS-WINDOW
412
101
1057
747
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-24
24
-24
24
0
0
1
ticks
30.0

BUTTON
61
234
124
267
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
222
314
394
347
initial-number-sheep
initial-number-sheep
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
44
314
216
347
initial-number-dogs
initial-number-dogs
0
100
5.0
1
1
NIL
HORIZONTAL

BUTTON
125
234
188
267
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
249
364
345
409
Current Score
calculate-score
3
1
11

SLIDER
40
172
212
205
crossover-probability
crossover-probability
0
1
0.53
0.01
1
NIL
HORIZONTAL

SLIDER
215
173
387
206
mutation-probability
mutation-probability
0
1
0.1
0.01
1
NIL
HORIZONTAL

PLOT
36
361
236
511
Score over time
Ticks
Score
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"pen-0" 1.0 0 -16777216 true "" "plot calculate-score"

PLOT
39
546
239
696
Average Dog Fitness
Ticks
Average Fitness
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot average-fitness"

TEXTBOX
81
530
231
548
NIL
11
0.0
1

SLIDER
230
122
402
155
cycleTime
cycleTime
0
500
100.0
1
1
NIL
HORIZONTAL

SWITCH
44
123
226
156
default-dog-movement
default-dog-movement
0
1
-1000

TEXTBOX
250
69
400
111
cycleTime is the amount of ticks before mutation / crossover happens
11
0.0
0

MONITOR
248
549
406
594
best-avg-fitness-overall
best-average-fitness-overall
17
1
11

MONITOR
247
654
408
699
best-avg-fitness-this-cycle
best-average-fitness-this-cycle
17
1
11

MONITOR
247
600
406
645
current-avg-fitness
average-fitness
17
1
11

MONITOR
250
412
344
457
average-score
average-score
0
1
11

BUTTON
241
234
365
267
Run for 10k steps
while [ticks <= 10000] [\n go\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
47
38
220
122
Note for this test model, turning off the default-dog-movement sets the behaviour to the best evolved chromosomes from the EVAC2-train file
11
0.0
1

@#$#@#$#@
# EVAC - Part 2 - Sheepdog training algorithm with evolutionary algorithms - Test file

## 5. Experimental evaluation


This evaluation goes through two sections, generic, surface level checks of the solution and a full run through of the evaluation procedure from above.

### Loading the chromosomes for testing and base level evaluation.

The genetic algorithm was ran for 100 cycles in the training netlogo file, the final chromosome values for the five dogs were taken from the model after it was ran and then transplanted into the dogs in this model in the setup function. Following this, all evolutionary methods were disabled so we can get a clear run of the behaviour using these evolved individuals.

Overall when the evolved behaviour is ran for 100 cycles it produces an average score of 360 over period. When the non-evolved behaviour is ran for the same period, it scores an average of 369. This is not much of an improvement and is definitely not a rigorous test of performance, as such we will employ the evaluation procedure described in the training file which will conclude the report.

### Running the evaluation procedure (t-test)


To collect the data I used the switch on the interface page which enables the default dog behaviour, following this I ran the model for 10k steps by pressing the convenient button that will stop the run after 10k steps, after which I recorded the average per-dog fitness from the command center. I repeated this while running the evolutionary algorithm and collected the per-dog fitness from this as well. The two arrays can be seen below, all values were rounded to 2dp.


avg-per-dog-fitness-default-behaviour (x1) = [1.19 1.29 1.21 1.40 1.46]


avg-per-dog-fitness-evolving-behaviour (x2) = [1.59 1.69 1.42 1.23 1.50]


note - the eventual avg fitness from the evolutionary behaviour varies from run to run, it could be higher or it could be lower.


I'll use this data below to run a t test as described earlier:


1 - Define hypotheses:


The null hypothesis (H0) for the test states that there is no significant difference between the mean of the two groups.


The alternative hypothesis (H1) states that there is a significant difference between the two population means.


2 - Calculate means, standard deviations and sample sizes:


sample size of x1 = 5


sample size of x2 = 5


mean of x1 = 1.310


mean of x2 = 1.486


std deviation of x1 = 0.105


std deviation of x2 = 0.157


3 - Calculate t-statistic


t-stat = mean-of-x1 - mean-of-x2 / sqrt((std-dev-of-x1 / 5) + (std-dev-of-x2 / 5))


t-stat = 2.08


4 - Calculate degrees of freedom and p value


df = n1 + n2 - 2 = 5 + 5 - 2 = 8


p-value = 1.860 (looked up from t table, one-tailed at significance level 0.05)


5 - Compare p value to t statistic


The test statistic falls above the p value i.e. 2.08 > 1.86. Therefore we reject the null hypothesis as there is a significant difference between the mean of the two groups. Therefore we can say that the evolved behaviour can perform slightly better at least. However, this is overshadowed by the fact that you can inspect the dogs at work and easily verify that they are not getting better over time. As such, this significance can probably be put down to the power of the representation in its unevolved form being more capable than the random walk behaviour. 


### Conclusion


It's clear that the implementation of this model was not up to scratch. So what could I have done better


- The "roles" idea is likely too complex for a genetic algorithm or not well thought out enough
- In the action implementation action such as chasing the nearest dog per tick leads to dogs repeatedly changing their closest target as they move. This led to indecisive dog behaviour when herding. With more time it may have been beneficial to rework this into an implementation that locked onto a chosen sheep for N cycles.
- I needed to return to my fitness, mutation, crossover and hatching and determine how to better implement it to my representation to achieve actual improvement. This implementation could have also greatly benefited from more advanced techniques such as fitness sharing, niching etc.


@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
