;Function Definitions
;helper function for set
(define (member x L);returns true if value x is member of L
(cond ((null? L) #f);returns false if empty
((equal? x (car L)) #t);else if x is the head, return true
(else (member x (cdr L)))));call the function on the tail of the list


(define (is-set? L)
(cond ((null? L) #t);base case where the list is now empty
((member (car L) (cdr L)) #f);base case where it's shown L is not a set
(else (is-set? (cdr L)))));general case where the tail of the list is checked

;returns a set from a list
(define (make-set L)
;use reverse to check if the last element is a member of front elements to preserve order
(cond ((member (car (reverse L)) (cdr (reverse L))) (make-set (reverse (cdr (reverse L)))))
((null? (cdr L)) list L);base case
(else (append (list (car L)) (make-set (cdr L))))));append head of L with accepted values

;returns #t if A is a subset of S
(define (subset? A S)
(cond ((null? A) #t)
((member (car A) S) (subset? (cdr A) S));recursively calls the tail if x is member
(else #f)))

(define (union A B)
(cond ((null? B) list A);if B is empty, list A
;if last element is in A union A with all other elements of B
((member (car (reverse B)) A) (union A (reverse (cdr (reverse B)))));else if head of B member A, union A tail of B
(else (append (union A (reverse (cdr (reverse B)))) (list (car (reverse B))))))); append union A with rest of B and last of B

;returns the intersection of two sets (note does not check if arg is set)
(define (intersection A B)
(cond
((null? A) list A);base case check if we arrived at last member
((member (car A) B)
(cons (car A) (intersection (cdr A) B)));cons head of A with accepted values
(else (intersection (cdr A) B))));

;hepler functions for binary tree
(define (val T)(car T));returns root
(define (left T)(cadr T));returns left subtree
(define (right T) (caddr T));returns right subtree

(define (tree-member? V T)
(cond
((null? T) #f)
((equal? V (val T)) #t)
(else (or (tree-member? V (left T)) (tree-member? V (right T))))))

;returns a list from the tree in inorder form
(define (inorder T)
(cond
((and (null? (left T)) (null? (right T))) (list (val T)))
((null? (left T))(cons (val T) (inorder (right T))))
((null? (right T)) (append (inorder (left T)) (list (val T))))
(else (append (inorder (left T)) (cons (val T) (inorder (right T)))))))

;returns a list from the tree in preorder form
(define (preorder T)
(cond
((and (null? (left T)) (null? (right T))) (list (val T)))
((null? (left T))(cons (val T) (preorder (right T))))
((null? (right T)) (append (preorder (left T)) (list (val T))))
(else (append (list (val T)) (preorder (left T)) (preorder (right T))))))

;finds instances of V in list L and all its sublists and deletes it
(define (deep-delete V L)
(cond
((null? L) list L)
((list? (car L)) (cons (deep-delete V (car L)) (deep-delete V (cdr L))));appends to L nested lists
((list? (car L)) (deep-delete V (append (car L) (cdr L))));appends to L nested lists
((equal? V (car L))(deep-delete V (cdr L)));deletes occurences of V
(else (cons (car L) (deep-delete V (cdr L))))));reconstruct L for valid numbers

;inserts new element into BST in proper position
(define (insert-BST V T)
(cond
((null? T) (list V '() '()))
((> V (val T)) (list (val T) (left T) (insert-BST V (right T))))
(else (list (val T) (insert-BST V (left T)) (right T)))))
;Assignment Definition
(define T '(13(5(1()())(8()(9()())))(22(17()())(25()()))));binary tree

;Test conditions (uncomment to test)
;-----------------Question 1a----------------------
;(is-set? '(1 2 5))
;(is-set? '(1 5 2 5))
;-----------------Question 1b----------------------
;(make-set '( 4 5 3 1 4 8 8))
;-----------------Question 1c----------------------
;(subset? '(1 3) '(1 2 3 4))
;(subset? '(1 2 3 4 5) '(1 2 3 4))
;-----------------Question 1d----------------------
;(union '(1 2 4 3) '(3 5 1 2))
;(union '(1 2 4 3) '( 5 7 8 ))
;-----------------Question 1e----------------------
;(intersection '(a b c d a) '(c d e f g))
;-----------------Question 2a----------------------
;(tree-member? 17 T)
;(tree-member? 18 T)
;-----------------Question 2b----------------------
;(preorder T)
;-----------------Question 2c----------------------
;(inorder T)
;-----------------Question 3----------------------
;(deep-delete 3 '(1 2 3 (4 3) 5 (6 (3 7)) 8))
;-----------------Extra Credit---------------------
(insert-BST 20 T)
