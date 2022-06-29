(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

global a: <procedure: me>
       b: <procedure: me>
| frame1: (set-value! a 10 'user)
|   | function: set-value!
|   | body: ((connector 'set-value!) new-value informant)
|   | connector: a
|   | new-value: 10
|   | informant: 'user
|   |
|   | frame2: ((a 'set-value!) new-value informant)
|   |   | frame3: (a 'set-value!)
|   |   |   | function: me
|   |   |   | body: (cond ((eq? request 'has-value?) ...)
|   |   |   |             ...)
|   |   |   | request: 'set-value!
|   |   | -----------
|   |   | value: false
|   |   | informant: false
|   |   | constraints: '()
|   |   | -----------
|   |   | function: set-my-value
|   |   | body: (cond ((not (has-value? me))
|   |   |              ...
|   |   |              (for-each-except setter
|   |   |                               inform-about-value
|   |   |                               constraints))
|   |   |              ...)
|   |   | new-value: 10
|   |   | setter: 'user
|   |   |
|   |   | frame4: (for-each-except setter
|   |   |                          inform-about-value
|   |   |                          constraints)
|   |   |   | function: set-my-value
|   |   |   | newval: 10
|   |   |   | setter: 'user
|   |   |   |
|   |   |   | frame5: (for-each-except setter inform-about-value constraints)