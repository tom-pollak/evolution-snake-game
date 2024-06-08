extensions [matrix]

breed [individuals individual]
breed [snakes snake]

globals [
  evaluate-ltot ; set in setup-evaluate-ltot
  ;; snakes vars
  current-generation
  ;; individuals vars
  next-ind-id
  chromosome-length
  weight-shapes
  weights
  directions-index
  ind-order
]

snakes-own [
  ;; FK to individual, this means I can have many snakes with the same
  ;; chromosome.
  individual-id
  last-move
  body
  food
  steps
  killed-snakes ; reward cooperation -- share fitness with killed snakes
  dead ; if we `die` snakes, then we can't get fitness at the end
  generation
]

individuals-own [
  id ; relates to snake:individual-id
  chromosome
  summed-fitness
  tot-food
  fitness
  avg-food
  avg-steps
  ;; NN
  w1
  w2
  ; w3
  ; w4
  b1
  b2
  ; b3
  ; b4
]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; WORLD PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; As `stop` dosen't work inside any function that isn't `go`, I use this function to cause a runtime error
;; to stop execution on detecting an error. This gives an ugly error message but info should be available in
;; the console.
to force-quit
  print "FORCE QUIT"
  let i [1]
  let j item 1 i ; force runtime error
end

to setup
  clear-all
  set current-generation 0
  set directions-index [90 270 180 0] ; up down left right
  set evaluate-ltot false
  if walls-on? [setup-walls]
  setup-individuals ; must be before snakes
  reset-world
  reset-ticks
end

;; final evaluation of ltot after training. Evaluates each snake individually, the best of which will be picked
;; in `run-evaluate-ltot` at end-of-generation
to setup-evaluate-ltot
  set evaluate-ltot true
  set ga? false
  set fitness-share? false
  print "disabling ga"
  print "disabling fitness sharing"

  set shuffle-inds? true
  print "enabling individual shuffling"

  print "each indiviudal gets 5 runs, to pick best ltot"
  print "temporarily setting ind-snakes: 5"
  set ind-snakes-per-gen 5

  print "calculating best individual..."
  reset-world

  create-ind-queue
  set ind-snakes-per-gen 1
end

;; runs the best snake for ltot, I can then export the ltot graph to do statisical test
to run-evaluate-ltot
  set evaluate-ltot false
  let sorted-inds sort-on [tot-food] individuals
  let best-inds sublist sorted-inds 0 n-snakes ; take best n-snakes indiviudals
  print word "best individuals: " best-inds

  print "saving weights to 'netlogo_pop_params_ltot_save.txt'"
  save-weights-to-file "netlogo_pop_params_ltot_save.txt"

  print word "setting popsize: " n-snakes
  print "setting generation 0"
  set popsize n-snakes
  set current-generation 0

  ask individuals with [not member? self best-inds] [die]
  reset-world

  print "starting evaluate ltot"

  ;; reset ltot plot so we can export it as csv
  set-current-plot "fitness"
  clear-plot

  set-current-plot "food"
  clear-plot

  set-current-plot "steps"
  clear-plot

  set-current-plot "ltot"
  clear-plot
end

to setup-snakes
  ;; else there would be extra individuals at the end that could not compete in a full game
  if (popsize * ind-snakes-per-gen) mod n-snakes != 0 [
    print "ERROR: n-snakes must a multiple of popsize * ind-snakes-per-gen (or vice-versa)"
    force-quit
  ]
  create-snakes n-snakes [ setup-snake ]
end

to setup-individuals
  ;; w1 b1 w2 b2 w3 b3 w4 b4 -- same as torch.state_dict()
  ; set weight-shapes [[32 24] [32 1] [32 32] [32 1] [16 32] [16 1] [4 16] [4 1]]

  ;; w1 w2 w3 w4 -- no biases
  ; set weight-shapes [[32 28] [32 32] [16 32] [4 16]]

  ;; w1 b1 w2 b2
  set weight-shapes [ [16 28] [16 1] [4 16] [4 1] ]

  set chromosome-length sum map numel weight-shapes
  print word "Number of parameters: " chromosome-length

  set weights init-weights

  ;; Loaded popsize != popsize set with slider
  if length weights != popsize [
    let new-popsize length weights
    print word "Setting popsize: " new-popsize
    set popsize new-popsize
  ]

  create-individuals popsize [
    setup-individual item next-ind-id weights
  ]
  create-ind-queue
end

to setup-walls
  ask patches [
    if (
      pycor = max-pycor
      or pycor = min-pycor
      or pxcor = max-pxcor
      or pxcor = min-pxcor
    )
    [ set-wall self ]
  ]
end

to go
  clear-vision-visualization
  if not inf-gens? and current-generation = n-gens [
    stop
  ]
  if count snakes = 0 or all? snakes [dead] [ end-of-run ]
  ask snakes with [not dead] [ snake-tick ]
  if random-float 1 < p-apple [ create-food ]
  tick
end

to _update-plots
  let fitnesses [fitness] of individuals
  let avg-fitness mean fitnesses
  let max-fitness max fitnesses

  let f [avg-food] of individuals
  let plot-avg-food mean f ; individuals already have var avg-food
  let plot-max-food max f

  let plot-avg-ltot plot-avg-food * n-snakes

  let s [avg-steps] of individuals
  let plot-avg-steps mean s
  let plot-max-steps max s

  set-current-plot "fitness"
  set-current-plot-pen "avg"
  plotxy current-generation avg-fitness
  set-current-plot-pen "max"
  plotxy current-generation max-fitness

  set-current-plot "food"
  set-current-plot-pen "avg"
  plotxy current-generation plot-avg-food
  set-current-plot-pen "max"
  plotxy current-generation plot-max-food

  set-current-plot "steps"
  set-current-plot-pen "avg"
  plotxy current-generation plot-avg-steps
  set-current-plot-pen "max"
  plotxy current-generation plot-max-steps

  set-current-plot "ltot"
  set-current-plot-pen "avg"
  plotxy current-generation plot-avg-ltot
end

to create-food
  let empty-patches patches with [empty-patch? self]
  ;; snake can fill the whole space not already populated with apples
  if count empty-patches != 0 [ set-food one-of empty-patches ]
end

;; when all the snakes are dead
to end-of-run
  let snake-ids [individual-id] of snakes
  let run-inds individuals with [member? id snake-ids]

  ask run-inds [ calc-game-fitness ]
  if empty? ind-order [ end-of-generation ]

  reset-world
  setup-snakes
  reset-ticks
end

;; when ind-queue is empty
to end-of-generation
  ifelse evaluate-ltot [ run-evaluate-ltot ]
  [
    ask individuals [ calc-final-fitness ]
    if fitness-share? [ fitness-share ]
    _update-plots
    if ga? [ do-ea ]
    set current-generation current-generation + 1
  ]
  create-ind-queue
  ask individuals [ reset-stats ]
end

to reset-world
  ask snakes [
    kill-snake
    die
  ]
  ask patches with [food? self] [set-empty self]
  clear-vision-visualization
end

to clear-vision-visualization
  ask patches with [vision? self] [ set-empty self ]
end

;; Creates a queue to be used to get an individual for each snake in a game. The queue ensures that every
;; individual has only n-snakes snakes.
to create-ind-queue
  let ind-ids flatten-2d [ n-values ind-snakes-per-gen [id] ] of individuals
  if shuffle-inds? [ set ind-ids shuffle ind-ids ]
  set ind-order ind-ids
end

to-report pop-ind-queue
  let ind-id first ind-order
  set ind-order but-first ind-order
  report ind-id
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;; PATCH PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to set-head [p]
  ask p [ set pcolor blue ]
end

to set-tail [p]
  ask p [ set pcolor green ]
end

to set-food [p]
  ask p [ set pcolor red ]
end

to set-empty [p]
  ask p [ set pcolor black ]
end

to set-wall [p]
  ask p [ set pcolor white ]
end

to set-vision [p]
  ask p [ set pcolor grey ]
end

to-report wall? [p]
  report walls-on? and [pcolor = white] of p
end

to-report food? [p]
  report [pcolor = red] of p
end

to-report vision? [p]
  report [pcolor = grey] of p
end

to-report empty-patch? [p]
  report not snake-here? p and not food? p and not wall? p
end

to-report snake-here? [p]
  ;; body is initialized as 0, so skip uninitialized snakes
  report any? snakes with [body != 0 and member? p body]
end

;; patch-ahead only works for the current turtle patch, this works for any arbitary patch
to-report patch-ahead-of [p d h]
  let _dx (d * cos h) ; 0 right => 1, 180 left => -1
  let _dy (d * sin h) ; 90 up => 1, 270 down => -1

  let _xcor ([pxcor] of p + _dx)
  let _ycor ([pycor] of p + _dy)

  report patch _xcor _ycor
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;; SNAKE PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;; CREATION

to setup-snake
  set hidden? true ; actual shape is hidden, color patches on the game.
  set food 0
  set steps 0
  set killed-snakes []
  set dead false
  set generation current-generation
  set individual-id pop-ind-queue
  create-snake
end


to create-snake
  set body []
  while [
    empty? body
    or member? true map other-snake-here? body
    or member? true map wall? body
  ] [
    remove-body
    setxy random-xcor random-ycor
    set-direction random-direction
    make-body
  ]
  foreach body set-tail
  set-head first body
end

to make-body
  ;; body includes head of snake (patch-here)
  set body []
  repeat initial-tail-length [
    set body fput patch-here body
    fd 1
  ]
  set body fput patch-here body
end

to remove-body
  foreach body set-empty
  set body []
end

;;; PATCH -- run in context of snake

to-report tail? [p]
  report member? p ([body] of self)
end

to-report other-snake-here? [p]
  report any? other snakes with [body != 0 and member? p body]
end

;;; DIRECTION

;; Only used to initialize snakes in a random direction, not movement :)
to-report random-direction
  report random 4
end

to set-direction [index]
  set last-move index
  let move-d item index directions-index
  set heading move-d
end

;;; ACTIONS

to snake-tick
  set-direction snake-forward
  let pa patch-ahead 1
  ifelse snake-here? pa or wall? pa [
    kill-snake
    ;; bit jank
    if other-snake-here? pa [
      let other-snake one-of other snakes with [member? pa body]
      ask other-snake [ add-kill myself ]
    ]
  ]
  [
    check-eat-food pa
    move-snake
  ]
end

to move-snake
  set steps steps + 1
  set-tail patch-here
  fd 1
  set-head patch-here
  set-empty last body
  set body fput patch-here but-last body
end

to check-eat-food [p]
  if food? p [
    set food food + 1
    set-empty p
    let new-p patch-ahead-of last body 1 (heading + 180)
    set body lput new-p body
    set-tail new-p
  ]
end

to kill-snake
  remove-body
  set dead true
end

to add-kill [s]
  set killed-snakes lput s killed-snakes
end

;;; SNAKE NN

;; This runs gets the snake's individual to make a decsion of what the snake
;; should do given its current state. individual-id is a FK for an individual
to-report snake-forward
  let state get-state
  let snake-id individual-id
  report [forward-pass state] of one-of individuals with [id = snake-id]
end

;; Input to individual's NN
to-report get-state
  report (sentence get-vision get-other-snakes-last-direction)
end

;; Mean direction for each of the other snakes
;; last-move is the index of the heading in directions-index
to-report get-other-snakes-last-direction
  let last-move-directions [0 0 0 0]
  let other-snakes other snakes with [not dead]
  if count other-snakes = 0 [ report last-move-directions ]
  let inc 1 / count other-snakes
  ask other-snakes [
    let cur-l item last-move last-move-directions
    set last-move-directions replace-item last-move last-move-directions (cur-l + inc)
  ]
  report last-move-directions
end

;; Transplanted from python sense_distance_direction
;; [1 0 0] -> wall / other snake?
;; [0 1 0] -> snake
;; [0 0 1] -> food
to-report get-vision
  ;; These are not the same as the python game, start at bottom left
  ;; Does it matter if you are consistent?
  let directions [
    [-1 0]  ; up
    [1 0]   ; down
    [0 -1]  ; left
    [0 1]   ; right
    [-1 1]  ; up right
    [-1 -1] ; up left
    [1 1]   ; down right
    [1 -1]  ; down left
  ]

  ;; +1 so that max range is shown up on vision
  let c 1 / (log (snake-vision-range + 1) 10)

  let px [pxcor] of patch-here
  let py [pycor] of patch-here

  let distances []
  foreach directions [dir ->
    let _dx item 0 dir
    let _dy item 1 dir
    let x px
    let y py
    let _distance 0
    let hit-wall false ; only used with walls-on

    ;; Will get transformed to 0
    let object-dist snake-vision-range + 1
    let tail-dist snake-vision-range + 1
    let food-dist snake-vision-range + 1

    ;; not <= because distance is evaluated at the start of the loop, not end
    while [_distance < snake-vision-range and not hit-wall] [
      set x x + _dx
      set y y + _dy
      set _distance abs (x - px) + abs (y - py)
      ;; Map wraps, so so does vision
      let current-patch one-of patches with [pxcor = x mod max-pxcor and pycor = y mod max-pycor]

      if wall? current-patch [
        set object-dist min list _distance object-dist
        set hit-wall true
      ]
      if food? current-patch [ set food-dist min list _distance food-dist ]
      if tail? current-patch [ set tail-dist min list _distance tail-dist ]
      if other-snake-here? current-patch [ set object-dist min list _distance object-dist ]
      if visualize-vision? and empty-patch? current-patch [ set-vision current-patch ]
    ]
    let vision (list object-dist tail-dist food-dist)
    ;; Scales distance 1 to (v + 1) to 1-0
    ;; This tends to inf at 0, but minimum distance is 1 (0 is on top of the snake)
    let transformed-vision map [dist -> 1 - (log dist 10) * c] vision
    ;; concatenate -- flattened list
    set distances sentence transformed-vision distances
  ]
  report distances
end

;;;;;;;;;;;;;;;;;;
;;; INDIVIDUAL ;;;
;;;;;;;;;;;;;;;;;;

to-report clone-ind [ind]
  let new-ind nobody
  create-individuals 1 [
    setup-individual [chromosome] of ind
    set new-ind self
  ]
  report new-ind
end

to setup-individual [c]
  set hidden? true
  set chromosome c
  load-weights-from-chromosome

  set id next-ind-id
  set next-ind-id next-ind-id + 1
end

to reset-stats
  set fitness 0
  set summed-fitness 0
  set tot-food 0
end

;;; GA

to do-ea
  let parents select-tournament individuals (popsize / 2)
  let offspring map clone-ind select-roulette individuals (popsize / 2)
  var-and offspring
  let new-pop sentence parents offspring
  ask individuals with [not member? self new-pop] [die]
end

to calc-final-fitness
  set fitness summed-fitness / ind-snakes-per-gen
end

;; an individual may have more than one snake with its chromosome, so uses mean
to calc-game-fitness
  let ind-id id
  let ind-snakes snakes with [individual-id = ind-id]

  let game-avg-food mean [food] of snakes

  let tf sum [food] of ind-snakes
  let f mean [food] of ind-snakes
  let s mean [steps] of ind-snakes
  let n-kills sum map length [killed-snakes] of ind-snakes

  set avg-food f
  set avg-steps s


  ;; With shuffle-inds on, this gives 75 steps to an apple
  ;; We need to subtract steps or else the GA gets caught in local optima of moving straight forward
  ;; until it hits its own tail.
  let fit (50 * f + 50 * game-avg-food - s) / (n-kills + 1) + 0.01

  set tot-food tot-food + tf
  set summed-fitness summed-fitness + fit
end

to fitness-share
  ask individuals [ set fitness fitness / fitness-deductible ]
end

;; zips two lists [1 2 3] [4 5 6] => [ [1 4] [2 5] [3 6] ]
to-report zip2 [l1 l2]
  report (map list l1 l2)
end

to-report euclidean-distance [l1 l2]
  if (length l1 != length l2) [
    error "ERROR: Lists must be of same length"
    force-quit
  ]
  let sum-squared-differences sum map [ [xs] -> (first xs - last xs) ^ 2 ] (zip2 l1 l2)
  let euc-dist sqrt sum-squared-differences
  report euc-dist
end

;; compute-f3 would not work as the values of the chromosomes are continuous floats
to-report fitness-deductible
  let ind-deduct 1
  foreach [self] of other individuals [ other-ind ->
    let dist euclidean-distance chromosome [chromosome] of other-ind
    if dist < share-sigma [
      let deduct 1 - (dist / share-sigma) ^ share-alpha
      set ind-deduct ind-deduct + deduct
    ]
  ]
  report ind-deduct
end

to var-and [inds]
  let shuf-inds shuffle inds
  let i 0
  while [(i + 1) < length inds] [
    let ind1 item i shuf-inds
    let ind2 item (i + 1) shuf-inds

    uniform-crossover ind1 ind2
    mutate ind1
    mutate ind2
    set i i + 2
  ]
end

to swap-gene [ind1 ind2 i]
  let gene1 item i [chromosome] of ind1
  let gene2 item i [chromosome] of ind2
  ask ind1 [ set chromosome replace-item i chromosome gene2 ]
  ask ind2 [ set chromosome replace-item i chromosome gene1 ]
end

to mutate-gene [ind i]
  let gene item i [chromosome] of ind
  let mut-gene gene + random-normal 0 mutsigma
  ask ind [ set chromosome replace-item i [chromosome] of ind mut-gene ]
end


to uniform-crossover [ind1 ind2]
  let i 0
  while [i < chromosome-length] [
    if random-float 1 < cxind [ swap-gene ind1 ind2 i ]
    set i i + 1
  ]
end

to mutate [ind]
  let i 0
  while [i < chromosome-length] [
    if random-float 1 < mutind [ mutate-gene ind i ]
    set i i + 1
  ]
end

to-report select-best [inds k]
  let sorted-inds sort-on [fitness] inds
  report sublist sorted-inds 0 k
end

;; Gets the index associated with a roulette roll
;; should only be called within select-roulette
;; This is its own function because netlogo dosen't have a way of breking `foreach`
;; so its a seperate function that return a value instead
to-report random-cum-select-index [xs]
  let tot-xs sum xs
  let norm-xs map [ i -> i / tot-xs ] xs
  let n random-float 1

  let cum-sum 0
  let i 0
  foreach norm-xs [ x ->
    set cum-sum cum-sum + x
    if cum-sum > n [ report i ]
    set i i + 1
  ]
end

to-report select-roulette [inds k]
  let ind-list [self] of individuals
  let fitnesses map [ i -> [fitness] of i ] ind-list
  let picked-inds []
  while [length picked-inds < k] [
    let ind-index random-cum-select-index fitnesses
    set picked-inds lput (item ind-index ind-list) picked-inds
    set fitnesses replace-item ind-index fitnesses 0 ; no pick twice
  ]
  report picked-inds
end

to-report random-non-repeating [n limit]
  let random-list []
  while [length random-list < n] [
    set random-list remove-duplicates sentence random-list (list random limit)
  ]
  report random-list
end

to-report _run-tournament-winner-index [fitnesses]
  let random-is random-non-repeating tournsize length fitnesses
  let fis map [ i -> item i fitnesses ] random-is
  let winner-i argmax fis
  report item winner-i random-is
end


to-report select-tournament [inds k]
  let ind-list [self] of individuals
  let fitnesses map [ i -> [fitness] of i ] ind-list
  let picked-inds []
  while [length picked-inds < k] [
    let winner-i _run-tournament-winner-index fitnesses
    set picked-inds lput (item winner-i ind-list) picked-inds
    set ind-list remove-item winner-i ind-list
    set fitnesses remove-item winner-i fitnesses
  ]
  report picked-inds
end

;;; NN FUNCTIONS

to-report relu [x]
 report max list x 0
end

to-report identity [x]
  report x
end

;; using argmax rather than softmax -> torch.mutlinomial
to-report argmax [xs]
  let max-val max xs
  let i 0
  foreach xs [ x ->
    if x = max-val [ report i ]
    set i i + 1
  ]
end

;; number of elements by shape, similar to torch.numel() but takes shape rather than actual tensor
to-report numel [_shape]
  report reduce * _shape
end

;; xs must be a flat list, rehsaped to m x n
to-report reshape [xs m n]
  let result []
  let i 0
  while [i < m] [
    let si n * i
    let row sublist xs si (si + n)
    set result lput row result
    set i i + 1
  ]
  report result
end

to-report flatten-2d [x]
  ;; flatten by repeatedly concatenating
  report reduce [[i j] -> (sentence i j)] x
end

;;; NN MODEL

to-report linear-layer [x w b activation]
  let wx matrix:times w x
  ;; b = -1 disables biases
  let pre-a ifelse-value b = -1 [ wx ] [ matrix:plus wx b ]
  if activation = "relu" [ report matrix:map relu pre-a ] ; runresult / run dosen't seem to work
  if activation = "identity" [ report matrix:map identity pre-a ]
  print word "ERROR: Activation function not found: " activation
  force-quit
end

to-report forward-pass [x]
  set x matrix:from-column-list (list x) ; 24 x 1
  set x linear-layer x w1 b1 "relu"
  set x linear-layer x w2 b2 "identity"
  set x item 0 matrix:to-column-list x ; [[x1 x2 x3 x4]]
  report argmax x
end

;;; WEIGHTS

to-report init-weights
  let w ifelse-value ga-init = "new pop"
  [ n-values popsize [ -> n-values chromosome-length [random-normal 0 1] ] ]
  [ load-weights-from-file weights-filename ]
  print "Loaded weights"
  report w
end

to save-weights-to-file [filename]
  carefully [ file-delete filename ] []
  file-open filename
  foreach [chromosome] of individuals [ c ->
    ;; concatenates genes together with " " and trims last space
    let line reduce [ [l g] -> (word l " " g) ] c
    file-print line
  ]
  file-close
  print word "Saved weights to: " filename
end

to-report load-weights-from-file [filename]
  file-open filename
  let data []
  while [not file-at-end?] [
    let line file-read-line
    set line (word "[" line "]")
    if line != [ "[]" ] [
      let row read-from-string line
      set data lput row data
    ]
  ]
  file-close
  report data
end

to-report load-weight-from-chromosome [i]
  let prev-w-shapes sublist weight-shapes 0 i ; excluding i
  let c-start-i sum map numel prev-w-shapes ; start weight index is after the size of all previous weights

  let w-shape item i weight-shapes
  let w-length numel w-shape
  let w-flat sublist chromosome c-start-i (c-start-i + w-length)
  let w-reshape reshape w-flat first w-shape last w-shape
  report matrix:from-row-list w-reshape
end

to load-weights-from-chromosome
  set w1 load-weight-from-chromosome 0
  set b1 load-weight-from-chromosome 1
  set w2 load-weight-from-chromosome 2
  set b2 load-weight-from-chromosome 3
end
@#$#@#$#@
GRAPHICS-WINDOW
193
17
474
299
-1
-1
13.0
1
10
1
1
1
0
1
1
1
0
20
0
20
1
1
1
ticks
30.0

BUTTON
19
25
85
58
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
108
26
171
59
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

SLIDER
12
123
184
156
p-apple
p-apple
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
12
257
185
290
n-snakes
n-snakes
1
20
4.0
1
1
NIL
HORIZONTAL

SLIDER
13
74
185
107
initial-tail-length
initial-tail-length
0
20
12.0
1
1
NIL
HORIZONTAL

CHOOSER
9
434
106
479
ga-init
ga-init
"new pop" "load pop"
0

SWITCH
422
319
527
352
walls-on?
walls-on?
1
1
-1000

INPUTBOX
400
360
457
420
mutind
0.025
1
0
Number

INPUTBOX
467
360
534
420
mutsigma
0.75
1
0
Number

SLIDER
9
164
184
197
snake-vision-range
snake-vision-range
0
20
4.0
1
1
NIL
HORIZONTAL

SWITCH
276
318
413
351
visualize-vision?
visualize-vision?
1
1
-1000

INPUTBOX
399
426
458
486
cxind
0.1
1
0
Number

SWITCH
165
373
255
406
ga?
ga?
0
1
-1000

PLOT
545
19
811
164
fitness
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"avg" 1.0 0 -2674135 true "" ""
"max" 1.0 0 -13345367 true "" ""

SLIDER
15
373
156
406
ind-snakes-per-gen
ind-snakes-per-gen
1
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
11
214
183
247
popsize
popsize
2
100
20.0
2
1
NIL
HORIZONTAL

SWITCH
7
501
133
534
fitness-share?
fitness-share?
0
1
-1000

SLIDER
14
317
158
350
n-gens
n-gens
1
500
500.0
1
1
NIL
HORIZONTAL

PLOT
543
173
811
323
food
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"avg" 1.0 0 -2674135 true "" ""
"max" 1.0 0 -13345367 true "" ""

PLOT
542
337
809
487
steps
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"avg" 1.0 0 -2674135 true "" ""
"max" 1.0 0 -13345367 true "" ""

SWITCH
165
318
268
351
inf-gens?
inf-gens?
0
1
-1000

INPUTBOX
469
427
533
487
tournsize
3.0
1
0
Number

INPUTBOX
209
427
389
487
weights-filename
final_weigths.txt
1
0
String

BUTTON
116
436
201
479
save-weights
save-weights-to-file weights-filename
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
542
502
810
652
ltot
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"avg" 1.0 0 -2674135 true "" ""

BUTTON
400
502
533
535
NIL
setup-evaluate-ltot
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
264
373
391
406
shuffle-inds?
shuffle-inds?
0
1
-1000

SLIDER
140
501
261
534
share-sigma
share-sigma
0
50
9.9
0.1
1
NIL
HORIZONTAL

SLIDER
269
502
396
535
share-alpha
share-alpha
0
1
0.5
0.01
1
NIL
HORIZONTAL

@#$#@#$#@
> Exam number: Y3891128

# Q1 Snake Representation

## 8-Way Vision

I used 8 way vision limited to 4 manhattan distance patches. This is implemented in the `get-vision` snake procedure. This is similar to the DEAP represenation in part 1, and is mainly transplanted from the `sense_distance_direction` function in Python.

For each direction vision returns a 3 length list with the minimum found distance to each of the  corrosponding attributes [other snake, tail, food]. If the attribute isn't found within the vision distance, the distance is set to equivalent of inf (see later normalization). This creates an 8x3 vision matrix, that is then flattened as input to the NN.

> To visualize what the each snake sees, turn `visualize-vision?` on.

### Distance Normalization

The distances are normalized to between 0 and 1 using log10. The log10 causes an exponential increase as the distance gets closer to the snake. This gives more precedence for the model to differentiate on objects closer to the snake and less on objects further away, which is useful because we'd rather differentiate between distances 1 and 2, rather than 3 and 4. I implemented the same technique for part 1 with a vision range of 16, which made a sizable improvement.

- As the distance to the snake approaches 0, the norm distance approaches 1. This will make more neurons fire the closer the object gets, which should be a good intrinsic bias for the small NN.
- The not found inf values from above get normalized to 0.

I normalize the distances using log10 and a coefficient based on vision range, s.t. at `snake-vision-range + 1, y = 0`. The `+ 1` is so that at maximum snake vision range, `y > 0`.

- `1 - (log10 x)`: at x = 1, y = 1, and as x tends to infinity y tends to 0. The distance will always be >= 1, as distance 0 is overlapping with the snake.
- Create the coefficient `c` to normalize distances based on vision range
	- `let c 1 / (log (snake-vision-range + 1) 10)`
	- `+1` so that max range shows up on vision
`1 - (log10 x) * c`: at x = 1, y = 1, at `x = max-vision-range`, `y = 0`

> I made a desmos graph to show how vision range effects the normalization:
> https://www.desmos.com/calculator/ilcw26rc4m

### Comparing One-Hot Grid Content to Vision

My priority was to limit the number of parameters of the NN as much as possible. Through my experiments with GAs I have found that more parameters usually make it more difficult to optimize due to the random nature of the mutations. This can often make large models perform worse than smaller ones. The most determining factor to the number of parameters is the number of inputs to the first layer. This is because in my NN designs every layer after the first contains less neurons and less parameters.

A full manhattan grid content contains 40 patches, and with a 3 length one-hot representation for each patch, this gives 120 inputs alone. To feed into a size 16 hidden layer, the number of parameters would be 120x16 + 16 = 1936. This is far too many and leads to slow performance and slow convergence.

8-way vision on the other hand only needs 8x3=24 inputs. The number of parameters for the first layer would be 400 parameters. The drawback of vision is that it will only see the closest food, tail, or snake in that direction, a.k.a it can't see the food behind another food.

It should be noted that this isn't a fair comparison however, as 8-way vision cannot see 16 of the 40 patches in the grid content. A more fair evaluation would be `(40-16)x3x16=1152` parameters. This has almost identical content to the vision, but with still almost 3x the parameters. To view every patch in the grid, I would have to use 16-way vision, which would a notable parameter bump.

Finally to note, while not used in this question, increasing the vision range using grid content polynomially increases the parameter size, while using vision the number of parameters stays constant.

## Other snakes' last move

To communicate other snakes move efficiently between snakes, I use a 4 length list, where each element represents a direction [up down left right]. I then add each of the other snake's directions s.t. the sum of the list is 1. In other words it gives the *mean direction* and is normalized so that every value is between 0 and 1.

> E.g. If there were 3 other snakes, 2 moving up and one moving down: `[0.66 0.33 0 0]`

## On not using current direction

I removed current direction as all inputs and outputs of the NN are absolute values. E.g. direction up is always index 0, no matter the snakes' direction. Further, the NN outputs an absolute direction, rather than left or right.

## NN Input

The above 2 inputs are flattened and concatenated, giving an input of 1x28.

## Entities

I use two seperate entities for the game:

- **Individual** Non-interactable entity that contains a chromosome and corresponding NN weights which describe the behaviour of a snake. These are evolved with GA, and kept through games & generations.
- **Snake** Fungible, dispensible, physical entity that plays the game, referring to their assigned individual to dictate their behaviour. The snake is deleted at the end of each game.
    - Uses individual-id as a foreign key to an individual in a many-to-one relationship.

This design gives a seperation of concerns between the evolution of the chromosome and the indvidual snakes playing the game. At each move the snake passes its current world state to their Indiviudal (snake-forward), which forward passes it through NN (forward-pass) to determine the snakes action.

The original reason I opted for this design is that each chromosome can control more than one snake. On rereading the exam brief I realize only one snake can have the same chromosome, so this behaviour can be changed using `ind-snakes-per-gen = 1` in the interface.

I ran my training using both multiple chromosomes per snake and only a single chromosome. The multiple chromosomes work together much better and lead to a smoother learning rate.  If multiple indiviudals are in a game together, it can become a kill or be killed environment that can be hard to optimize. However, as given in the brief, I used a single individual to a single snake for training.

You can see this in the training screenshot below, at ~1000 generations the fitness of the entire population drops dramatically. In the training all snakes change from moving upwards to left-to-right. I suspect this was because a snake figured out that he could cut off every snake by going in an opposite direction. This adversial technique seriously harmed the overall population.

- Image of training with `ind-snakes-per-gen = 1` https://ibb.co/HFG85B3

Final `ind-snakes-per-gen = 1` snake weights at `final_weights.txt`

# Q2

## Game Parameters

-  As explained above, to have each snake use their own chromosome, set `ind-snakes-per-gen` to 1. Also ensure `shuffle-inds?` is on.
	- Conversly, to have individuals train with themselves, set `ind-snakes-per-gen` to `n-snakes` and turn `shuffle-inds?` off.
- Set `n-gens` to the number of gens to train for, or turn `inf-gens?` on.
- Adjust game parameters:
	- `n-snakes`: number of snakes in a game
	- `initial-tail-length`: I start with 12 which is relatively high but speeds up training as snakes cannot use the strategy of stay straight forward collecting apples that spawn ahead of them for as long.
	- `p-apple`: probability of apple per game tick


## Creating new population

Set `ga-init` to "new pop".

- Set `popsize` to the number of chromosomes trained in the GA. *This must be a multiple of `n-snakes`*

## Loading population

Set `ga-init` to "load pop"

- Loads weights from `weights-filename`
- Will set `popsize` to the number of indiviudals in the loaded file.

## Simulation objectives

The overarching objective of the simulation is to develop a population of snakes that by the end of the game reaches a total length Ltot that is as large as possible.

To do this, we need the snakes to increase the length of their tails while also cooperating with the other snakes in the game. We use the fitness function described below, but a summary would be a mix of own score, average game score and a dividing multiple of the number of other snakes killed. This leads to a fairly cooperative strategy but as mentioned in Q1 the enviourment can tend to kill or be killed. Towards the end of the training run the snakes had learned to cooperate and move with each other, rather than against.

Once the training has ended, a second stage started where we evaluate the best snakes to use for the Ltot evaluation. This is so we can have a large population of snakes while the number of snakes in the game can remain a factor of that. This can lead to more population diversity, and is an advantage of the individual entity defined above.

The stage evaluates every individual and keeps the top `n-snakes` indiviudals. The ltot graph can then be exported to an csv to get the data to be statistically evaluated.

# Q3

## Feed-forward Neural Network

I use a feed-forward NN as a way to learn and modify the behaviour of the snake. The shape of the NN can be modified easily by changing `weight-shapes`. The current NN looks as follows:

```
;; w1 b1 w2 b2
set weight-shapes [ [16 28] [16 1] [4 16] [4 1] ]
```

This describes a 28 input, NN, which is passed to a hidden layer of 16 neurons with biases, that is passed to the 4 length output output layer, with biases. The weights are loaded from the individual's chromosome, which a flat list containing all of the parameters of the NN. The final output is argmaxed to give the direction.

> I use the matrix extension to matrix multiply the weights and inputs and add the biases for each layer in the MLP.

## GA

To adapt the NN, we can modify the chromsome list and load weights from the new chromosome. This adaption uses a GA.

For each new offspring, I apply both uniform crossover and mutation to their chromosome in a `var-and` algorithm. I intially used a `var-or` in both netlogo and part 1 with DEAP but I found applying both procedures better increased training time. I kept my mutation rate small, 0.025 (4 genes mutated on average) while leaving my crossover rate much higher at 0.1. I believe this helps continously adapt the population.

I used a roulette style selection when picking the indiviuals to crossover, my intuition behind this was that the best indiviudals will be the ones to share their genes with all the other indiviudals. I used uniform crossover to stop the individuals converging too quickly on each other, I found using one or two crossover did this quite quickly, this may be due to the roulette style selection.

## Fitness Sharing

I used fitness sharing with euclidean distance to stop indiviudals converging on each other, after adjusting the the alpha hyperparameter I settled on 10. Randomly initialized chromosomes have a distance of ~30, and I found nearing 10 the snakes started acting quite similar to each other. This stops behaviour but slows down each generation somewhat.

## Evolutionary Algorithm

For simplicity, I wanted to only evaluate the indiviudals at the start of each generation, as it I thought it would be quite complex evaluating the offpring independantly in the EA function.

To do this I took 2 samples of tournament selection, each 50% of the population, then ran `var-and` on one half to get the offspring, while leaving the other half unaffected. I then combined the two halves to create the population for the next generation.

## Fitness

My current fitness is `(50 * f + 50 * game-avg-food - s) / (n-kills + 1) + 0.01`


`game-avg-food` is the average food of all the snakes in that game. This optimizes for a snake getting its own food while also keeping the average score high.

I negate the number of steps because a very common local optima at the start of an evolution training run is to move straight ahead without turning, collecting food as it spawns in front of you until they hit their own tail or get cut off. Negating steps means that snakes will lose fitness if they obtain food less than every 50 steps, which shouldn't be the case unless they have a very step heavy strategy like the one above.

`n-kills` is the number of other snakes the current snake has killed, and is divided of the food score to decentivize killing other snakes. I don't think is perfect however as it is necessary at some point for a snake to kill another snake if there's no room left

The final 0.01 is to prevent 0 division errors.

# Q4

I will be evaluating Ltot as my primary measure, and test if there is a statistically significant difference in score of an adapted population over a non-adapted population.

## Evaluating Ltot

The button `setup-evaluate-ltot` will finish training and start the evaluation process. This will take the top `n-snakes` individuals, sorting by their total ltot. It will then backup the population to "netlogo_pop_params_ltot_save.txt" It will then remove all but the best individuals and evaluate them without GA. The ltot results can be exported from the ltot graph, which will be cleared beforehand to ensure clean data.

Run this at the end of training. Leave this to run the evaluation for a sufficient (e.g. 500) number of runs to obtain (more confident) results.

This is evaluated with the following game parameters:

- `initial-tail-length` 12
- `n-snakes` 4
- `popsize` 20
- `ind-snakes-per-gen` 1 (one snake per chromosome)

I have not included the intital-tail-length in the ltot score, letting it start from 0 instead as this is a constant for every snake and will not affect the test.

## Comparing to Non-adaptive behaviour

Non adaptive behaviour can be created by initializing a new random population with `ga?` set to off to disable learning. I followed Q2 Creating a new population and used `setup-evaluate-ltot` in the same way as specified above, the two ltot scores can be compared for statistical significance as described below.

## Comparing statistical significance

I have two populations, one trained using the GA mechanism and one non-adaptive population which has not been trained. From evaluating ltot above, I have a list of ltot scores from each population. I will test whether the two distributions are statistically significant

Assumptions:

- All game scores in each of the two lists are independant

This is true as each snake is initialized in a random new position not affected by previous generations. Further, in the evaluation process ga? is off, so the population is unaffected between generations.

## Resampling the data

The scores do not follow a normal distribution, but we can calcualte a mean score given 3 runs by resampling the single run data into bins of 3 runs. For each of the bins we take the mean score. We can do this as each run of a population is independant from each other. I will show this in the Jupyter Notebook in Q5.

## Picking a statistical test

The resampled game scores are not normally distributed, and do not have equal variances. Therefore I should use a non-parametric test to evaluate the statistical significance. In this case, I will use the Mann-Whitney U test because it is suitable for the above assumptions. I cannot use a ttest as the two samples are not normal distributions.

### Mann-Whitney Test

Null Hypothesis (H0): There is no significant difference between the adaptive population and non-adaptive population Ltot scores.

Alternate Hypothesis (H1): There is a significant difference between the two population's Ltot scores.

I am using a significance level of 0.05. This is a two-tailed test as I will assess if the adapted population has improved / regressed, so I must half my significance level for each tail.

I have implemented this statistical test in the attached jupyter notebook with scipy.


# Q5

The analysis, resampling and statisical tests are available in the jupyter notebook: "stat_anlysis_ltot.ipynb".

The outcome was a **rejection of the null hypothesis** and a **U-statistic of 100000000**. The results and findings are available in the jupyter notebook
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

invisible
false
0

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
