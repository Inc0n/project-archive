
(lambda ⟨vars⟩
  (define u ⟨e1⟩)
  (define v ⟨e2⟩)
  ⟨e3⟩)

would be transformed into

(lambda ⟨vars⟩
  (let ((u '*unassigned*)
        (v '*unassigned*))
   (set! u ⟨e1⟩)
   (set! v ⟨e2⟩)
   ⟨e3⟩))

'*unassigned*