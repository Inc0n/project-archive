(defun parse-1 (c)
  (case c (#\. 're/.)
          (#\* 're/*)
          (#\+ 're/+)
          (#\? 're/?)
          (#\{ 're/{})
          (#\[ 're/[])
          (#\( 're/round[])
          (#\| 're/or)
          (#\$ 're/tail)
          (#\^ 're/head)
          (#\\ '(re/next))
          (t (list 're/char c))))

(defun analysis (string)
  (mapcar #'parse-1 (coerce string 'list)))