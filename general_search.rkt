#lang racket
(provide GeneralSearch)
(provide tail-append)
(provide goal-state?)
(provide compose-gs-predicate)

; Author: Stephen Bothwell
; Date (last updated): 2/20/2019
; Course: CSCI370 - Artificial Intelligence
; Professor: Dr. Michael Goldweber
; Assignment: Scheme Homeworks 4+

; Given below is the generalized search algorithm and the functions necessary to support or to augment it.
; The GeneralSearch algorithm is thoroughly explained, so I will not rehash it here. However, it is at the heart of this document.
; The member? and reverse methods are helper functions to the GeneralSearch algorithm.
; The goal-state? and compose-gs-predicate work together to take a goal state and pre-parameterize goal-state? with the given goal-state.
; This makes goal-state? able to handle varying goal states without having to change variable names.
; Finally, the tail-append algorithm can be used to simulate BFS-based algorithms, including BFS itself.
; The tail-append algorithm has been provided here rather than elsewhere for convenience and due to its simplicity.

; This is the generalized search algorithm. It takes seven parameters and has four conditions. We will go through each of these. We start with the parameters.
; params:
; 1. The first parameter is paths, which is a list of lists of nodes, which, in themselves, are lists. Each list of nodes is called a path.
; These paths, as they grow, are ordered from their end to their beginning; that is, up until they are reversed, the paths are stored from the latest state to the start state.
; 2. The second parameter is visits, which stores all nodes that have been seen as the car of a path. This list is used to avoid running into cycles in the state space.
; 3. The third parameter is goal-state?, which is a predicate that checks whether the car of a path meets the criterion of the goal-state.
; 4. The fourth parameter is the successor function, which takes the latest node of a path and generates all potential paths which stem from that node and path.
; 5. The fifth parameter is the count, which is a number increases progressively based on how many unique nodes are explored and expanded upon.
; 6. The sixth parameter is the search-controller function, which is a function that defines what search algorithm is being done. Currently, DFS and BFS are supported.
; 7. The last parameter is the path-printer function, which is a function that prints a path more readably, interpreting the more calculatable but abstracted nodes.
;
; The conditions below are as follows:
; 1. The first condition checks whether paths is empty. If it is, then this means that no valid state reaches the goal-state. It lets the user know that the search has failed.
; 2. The second condition checks whether the first path has reached the goal-state. If so, the count and path-printer are used to display goal-related information.
; 3. The third condition checks whether the first node of the first path is a member of visits. If so, the algorithm skips this path and moves on to the next one by passing cdr paths to GeneralSearch.
; This avoids cycles in the state space.
; 4. The last condition takes the first non-goal path (which is the first path in paths) and passes the appropriate paths extending from that path onto paths in the way that search-controller dictates.
; Then, the rest of the parameters to GeneralSearch, along with this updated paths list, are passed to GeneralSearch for another round of searching.
;
; return: a statement of whether the search algorithm has succeeded at the search or not. If the algorithm succeeds, then the number of nodes expanded upon and a description of the path are also displayed.
(define GeneralSearch
  (lambda (paths visits goal-state? successor count search-controller path-printer)
    (cond
      ((null? paths) (display "No valid goal state has been found."))
      ((goal-state? (car paths)) (display "A valid goal state has been found.") (newline)
                           (display "Number of Nodes Explored: ") (display count) (newline)
                           (display "Successful Path: ") (newline) (path-printer (reverse (car paths))))
      ((member? (caar paths) visits) (GeneralSearch (cdr paths) visits goal-state? successor count search-controller path-printer))
      (else (GeneralSearch (search-controller (successor (car paths)) (cdr paths)) (cons (caar paths) visits) goal-state? successor (+ count 1) search-controller path-printer)))))

; This method was from the first problem of Homework 2.
; This method reverses the elements of a list (lst) and returns the list with the elements reversed.
; It appends the reverse of the cdr of lst to the car of lst.
; params: a list (lst), empty or nonempty. 
; return: the list with its elements having been reversed in their order.
(define reverse
  (lambda (lst)
    (cond
      ((null? lst) lst)
      (else (append (reverse (cdr lst)) (list (car lst)))))))

; This method was the first problem from Homework 1.
; This method is a predicate that states whether a given element (elem) is inside of a given list (lst).
; The method recursively cycles through the list using cdr until it finds a car that is equal to elem or until the method runs out of elements to search.
; params: an element (elem) and a list (lst) in that order.
; return: a boolean value stating whether the given element (elem) is in the list (lst). This method returns #t if the element is in the list.
(define member?
  (lambda (elem lst)
    (cond
      ((null? lst) #f)
      ((equal? elem (car lst)) #t)
      (else (member? elem (cdr lst))))))

; This method uses append with two lists (list-1, list-2) in a regular manner except that it reverses the order in which they are joined together.
; That is, list-2 is joined to the front of list-1, rather than the other way around. 
; params: two lists (list-1, list-2) that can be used with cons.
; return: a single list with the two items joined together with cons in the opposite order.
(define tail-append
  (lambda (list-1 list-2)
    (append list-2 list-1)))

; This method takes a path (current-path) and checks its first node (i.e. the path's latest addition) against another node that is the goal of the search (goal-state).
; It does so to determine whether the two are equivalent and returns a boolean accordingly.
; params: a path (current-path) to be checked against the goal state.
; return: a boolean of whether current-state and goal-state are equal. True, of course, denotes their equality.
(define goal-state?
  (lambda (current-path goal-state)
    (equal? (car current-path) goal-state)))

; This function composes the goal-state? function by pre-parameterizing it with its goal-state.
; params: a function (goal-state?) that determines whether a state is the goal of the current state and a node (goal-state) which is the goal of the search.
; return: a function of one parameter that takes a path and determines whether it has reached the goal state.
(define compose-gs-predicate
  (lambda (goal-state? goal-state)
    (lambda (current-path)
      (goal-state? current-path goal-state))))