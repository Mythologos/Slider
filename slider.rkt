#lang racket
(require "./general_search.rkt")

; Author: Stephen Bothwell
; Date (last updated): 9/13/2019
; Course: CSCI370 - Artificial Intelligence
; Professor: Dr. Michael Goldweber
; Assignment: Homework 5 - The 8-Puzzle Problem

; This file contains methods that specifically handle portions of the Slide Puzzle problem.
; Central to these algorithms is the format of a node. I have represented the node as a flattened list.
; That is, my representation contains nonnegative whole numbers with a number representing each tile.
; The number "0" represents a blank tile, whereas all other numbers represent different tiles.
; An 8-Puzzle would appear as follows, for example: (1 2 3 8 0 4 7 6 5). Thus, "0" is the (2, 2) entry of the slide puzzle.
; Other puzzles sizes can be used if and only if the slide puzzle size is n-by-n--that is, it is effectively a square matrix.
; (To see this, try start-state-10 and goal-state-3 in any of the search algorithms).

; Next, I will discuss the methods in this file and how they are divided up.
; The critical successor method uses various helper methods to get it to work. These are np-mapper, node-creator, and find-index.
; After the index of the blank is attained, node-creator discerns what types of moves are legal based on the blank's position.
; It uses leftward-move, rightward-move, upward-move, and downward-move methods to generate nodes accordingly
; These methods encode specific movements for the slide puzzle and are called depending on where the blank is in the slide puzzle.
; They use the swapper and subswap methods to do so.
; After a list of new nodes has been created, np-mapper joins them all to separate copies of the path used to derive them.

; Another group of methods is headed by path-printer. Its job is to intelligibly display information about a path.
; That is, although the representation is a flattened list, the path-printer method displays the slide puzzle as a grid and makes the path more readable.
; It uses the helper methods interpret-node and print-row to do so.

; After this, the compose-overall-sort and compose-misalignment functions work with the bestfs-sort and bnb+-sort functions.
; The former functions utilize the goal-state to pre-parameterize the latter two functions such that the goal state can be used to calculate misalignment.
; Misalignment, here, is a heuristic defined as the number of tiles which the latest state in a path has in incorrect locations.
; Correct locations are defined as the locations those tiles are in as per the given goal state.
; The functions bestfs-sort and bnb+-sort use the second of the two composition functions to compose their helper functions, calculate-misalignment and bound-misalignment.
; Both of these helper functions use count-bad-tiles to do their dirty work in counting misaligned tiles.

; Finally, the slide-puzzle-dfs, slide-puzzle-bfs, slide-puzzle-bestfs, and slide-puzzle-bnb+ methods perform Depth-First Search, Breadth-First Search, Best-First Search, and Branch-and-Bound+ Search, respectively.
; These all, of course, have the ultimate aim of solving the Slide Puzzle problem.
; Each of these methods can be parameterized. However, auto-dfs, auto-bfs, auto-bestfs, and auto-bnb+ methods have also been provided to pre-parameterize such methods.
; They only require initial and final states to run. Finally, the run-all-sorts method similarly takes an initial and final state, but performs all sorting algorithms in one shot.
; It uses the same start state and end state for all of them and provides the appropriate print formatting for visibility.

; Specific comments on methods and implementation details can be viewed below.

; This method is the successor function for the Slide Puzzle problem.
; It takes a path (path) and returns all paths which could be formed legally from the path's current position.
; It uses the helper methods np-mapper, node-creator, and find-index to do so.
; params: a nonempty list of nodes (path).
; return: a list of paths that legally extend from the path given as an argument.
(define successor
  (lambda (path)
    (np-mapper (node-creator (car path) (find-index 0 (car path)) (sqrt (length (car path)))) path)))

; This method was adapted from Homework 2's Cartesian Product method.
; The np-mapper, or "node-path mapper," method takes two lists--one of nodes and one of a legal path (nodes, path).
; It returns a list that consists of each node connected to the "end" of a distinct version of path.
; If there is no element in nodes, an empty list is returned; no paths should be added.
; params: a list of nodes (nodes), which can be empty or nonempty, and a list which contains the path relevant to the nodes (path).
; return: a single list containing lists which represent the possible paths stemming from the addition of each node in nodes to the given path.
(define np-mapper
  (lambda (nodes path)
    (cond
      ((null? nodes) '())
      (else (cons (cons (car nodes) path) (np-mapper (cdr nodes) path))))))

; This method creates all the nodes that can be derived from a given node (node).
; It uses the location where the "0" is (zero-index) and the length of the row of the slide puzzle (row-length) to determine the node generation that needs to occur.
; params: a state of the slide puzzle (node), an index of where the "0" is located (zero-index), and the length of a row of the slide puzzle (zero-index) 
; return: a list of all nodes that are adjacent states to the given node in the slide puzzle.
(define node-creator
  (lambda (node zero-index row-length)
    (append (leftward-move node zero-index row-length)
            (append (rightward-move node zero-index row-length)
                    (append (upward-move node zero-index row-length)
                            (downward-move node zero-index row-length))))))

; This method is an adaption of the fourth problem on Homework 2.
; This method is a helper method for np-mapper. It takes a value (item) and a node (node).
; Using these, the method determines the index of item in node by recursively adding up ones until it is found and searching the node until the value is found.
; Due to the constraints of the rest of the program, an index should always be found.
; params: a value to be searched for (item) and a list in which it will be searched for (node).
; return: an index stating where item is in node.
(define find-index
  (lambda (item node)
    (cond
      ((equal? item (car node)) 0)
      (else (+ 1 (find-index item (cdr node)))))))

; This method takes a node (node), the length of a row (row-length), and the index of the zero (zero-index) and
; generates a node that simulates moving the given zero to the left in the slide puzzle, should such a simulation be valid.
; params: a given node whose leftward adjacent state will be generated (node), the length of a row in the slide puzzle (row-length),
; and an index of where the zero is in the node (zero-index).
; return: a state that simulates a leftward movement of the zero in the given node.
(define leftward-move
  (lambda (node zero-index row-length)
    (cond
      ((integer? (/ zero-index row-length)) '())
      (else (list (swapper (list-ref node (- zero-index 1)) node))))))

; This method takes a node (node) and the index of the zero (zero-index) and
; generates a node that simulates moving the given zero to the right in the slide puzzle, should such a simulation be valid.
; params: a given node whose rightward adjacent state will be generated (node), the length of a row in the slide puzzle (row-length),
; and an index of where the zero is in the node (zero-index).
; return: a state that simulates a rightward movement of the zero in the given node.
(define rightward-move
  (lambda (node zero-index row-length)
    (cond
      ((integer? (/ (+ zero-index 1) row-length)) '())
      (else (list (swapper (list-ref node (+ zero-index 1)) node))))))

; This method takes a node (node), the index of the zero (zero-index), and the length of a row in the slide puzzle (row-length).
; It generates a node that simulates moving the given zero down in the slide puzzle, should such a simulation be valid.
; params: a given node whose downward adjacent state will be generated (node) and an index of where the zero is in the node (zero-index).
; return: a state that simulates a downward movement of the zero in the given node.
(define downward-move
  (lambda (node zero-index row-length)
    (cond
      ((>= zero-index (- (length node) row-length)) '())
      (else (list (swapper (list-ref node (+ zero-index row-length)) node))))))

; This method takes a node (node), the index of the zero (zero-index), and the length of a row in the slide puzzle (row-length).
; It generates a node that simulates moving the given zero up in the slide puzzle, should such a simulation be valid.
; params: a given node whose upward adjacent state will be generated (node) and an index of where the zero is in the node (zero-index).
; return: a state that simulates an upward movement of the zero in the given node.
(define upward-move
  (lambda (node zero-index row-length)
    (cond
      ((< zero-index row-length) '())
      (else (list (swapper (list-ref node (- zero-index row-length)) node))))))
    
; This method is an adaption of the ninth problem on Homework 2.
; This method swaps a value (swap-val) found in a node (node) with the zero also found in said node.
; It uses the subswap helper method to do so.
; params: a value to be swapped (swap-val) and a node in which the swap shall occur (node).
; return: a node with the instances of swap-val and 0 present in the lists having been switched around.
(define swapper
  (lambda (swap-val node)
    (cond
      ((null? node) node)
      (else (cons (subswap swap-val (car node)) (swapper swap-val (cdr node)))))))

; This method is a helper method for swapper. It takes the value to-be-swapped (swap-val) and checks it against a value that was on the list (lst-val).
; If lst-val equals swap-val or 0, the other of swap-val or 0 is returned. If not, lst-val is returned, as no swapping needs to be done.
; params: one value (swap-val) to check the value in the list (lst-val) against for swapping.
; return: one of the two parameters or 0 depending on whether a swap needs to occur (swap-val, 0) or not (lst-val).
(define subswap
  (lambda (swap-val lst-val)
    (cond
      ((equal? swap-val lst-val) 0)
      ((equal? 0 lst-val) swap-val)
      (else lst-val))))

; This method takes a path--a list of nodes (path)--and prints out what each one is in separate messages.
; It simply uses recursion to loop through the path and processes each node until it is finished with the interpret-node method.
; params: a path (path) containing nodes of the given format for the Slide Puzzle problem.
; return: messages concerning the contents of each node in the path.
(define path-printer
  (lambda (path)
    (cond
      ((null? path) (display "And that's it! Congratulations!"))
      (else (display "----------") (interpret-node (car path) (sqrt (length (car path))))
            (newline)
            (path-printer (cdr path))))))

; This method takes a node (node) of the appropriate format for the Slide Puzzle problem and prints it by row using the length of each row as a metric (row-length).
; It uses the print-row helper method to do so.
; params: a node (node) of the appropriate formatting--the one specified earlier in this document--and the length of a row of the slide puzzle (row-length).
; return: a string which represents the contents of the node.
(define interpret-node
  (lambda (node row-length)
    (cond
      ((equal? (length node) row-length) (print-row node row-length row-length))
      (else (interpret-node (print-row node row-length row-length) row-length)))))

; This method is a helper method to interpret-node. It takes a node (node) and prints across rows.
; It keeps track of the length of a row (orig-row-length) and where in the row the algorithm currently is (row-left) in order to do so.
; params: the node to be printed (node), the length of a slide-puzzle row for the node (orig-row-length), and how many elements in a row there are left to print (row-left).
; return: a print of the row of the slide puzzle state represented by node.
(define print-row
  (lambda (node orig-row-length row-left)
    (cond
      ((equal? row-left 0) node)
      ((equal? (modulo (length node) orig-row-length) 0) (newline) (display (car node)) (display " ") (print-row (cdr node) orig-row-length (- row-left 1)))
      (else (display (car node)) (display " ") (print-row (cdr node) orig-row-length (- row-left 1))))))


; This function takes a sorting algorithm (sort-algorithm; i.e. bestfs-sort and bnb+-sort) and pre-parameterizes it with the goal state (goal-state).
; It is the first of two levels of indirection required to pre-parameterize the goal-state due to the constraints of the sort function.
; params: a sorting algorithm to be pre-parameterized (sort-algorithm) and a node representing the goal state (goal-state).
; Note that the sorting algorithm must have a parameter available for the goal-state.
; return: a function that is the same as the initial sorting function but with its goal-state pre-parameterized.
(define compose-overall-sort
  (lambda (sort-algorithm goal-state)
    (lambda (list-1 list-2)
      (sort-algorithm list-1 list-2 goal-state))))

; This function takes a misalignment-counter algorithm (misalignment-counter; i.e. bound-misalignment and calculate-misalignment) and pre-parameterizes it with the goal state (goal-state).
; It is the second of two levels of indirection required to pre-parameterize the goal-state due to the constraints of the sort function.
; params: a counting algorithm to be pre-parameterized (misalignment-counter) and a node representing the goal state (goal-state).
; Note that the misalignment counter must have a parameter available for the goal-state.
; return: a function that is the same as the initial counting function but with its goal-state pre-parameterized.
(define compose-misalignment
  (lambda (misalignment-counter goal-state)
    (lambda (path)
      (misalignment-counter path goal-state))))

; This function implements the sorting that makes bestfs-search best-first.
; Having been pre-parameterized by compose-overall-sort, it takes a goal state as one of its arguments (goal-state).
; It also takes two lists that are combined in the manner of breadth-first search (list-1, list-2).
; Then, this combined list is sorted by how many tiles remain unaligned with the goal state in the path's latest state.
; params: two lists to be combined and sorted (list-1, list-2) and a goal state to be a measure for the sorting process (goal-state).
; return: a list of paths sorted based upon how closely aligned individual tiles are with the goal-state.
(define bestfs-sort
  (lambda (list-1 list-2 goal-state)
    (sort (tail-append list-1 list-2) < #:key (compose-misalignment calculate-misalignment goal-state))))

; This function is mainly a preparatory function. It primes parameters for the count-bad-tiles function.
; All that it needs to do is get the first node in the path parameter. This cannot be easily done anywhere else, so having this intermediary method is convenient.
; params: a path (path) and a goal state (goal-state) to be passed to the count-bad-tiles function in accordance with its own parameters.
; return: the result of count-bad-tiles, a metric concerning how many tiles do not match up between the current state of the path and the goal state.
(define calculate-misalignment
  (lambda (path goal-state)
    (count-bad-tiles (car path) goal-state)))

; This function takes a node (node) and is pre-parameterized with a goal state (goal-state).
; With these, it determines how many of the tiles in node line up with the places they ought to be in to achieve the goal-state.
; params: a node (node) and a goal state (goal-state) to be compared for tile alignment.
; return: a count of how many tiles in node line up with those in goal-state.
(define count-bad-tiles
  (lambda (node goal-state)
    (cond
      ((null? node) 0)
      ((not (equal? (car node) (list-ref goal-state (- (length goal-state) (length node))))) (+ 1 (count-bad-tiles (cdr node) goal-state)))
      (else (count-bad-tiles (cdr node) goal-state)))))

; This function implements the sorting that makes branch-and-bound+ search perform according to its own algorithm.
; Having been pre-parameterized by compose-overall-sort, it takes a goal state as one of its arguments (goal-state).
; It also takes two lists that are combined in the manner of breadth-first search (list-1, list-2).
; Then, this combined list is sorted by the sum of how many tiles remain unaligned with the goal state in the path's latest state and the number of nodes in the path.
; params: two lists to be combined and sorted (list-1, list-2) and a goal state to be a measure for the sorting process (goal-state).
; return: a list of paths sorted based upon the sum of how many individual tiles of the latest node align with the goal-state and the number of nodes in the corresponding path.
(define bnb+-sort
  (lambda (list-1 list-2 goal-state)
    (sort (tail-append list-1 list-2) < #:key (compose-misalignment bound-misalignment goal-state))))

; This function is mainly a preparatory function. It primes parameters for the count-bad-tiles function.
; It adds the length of the path to the result of the count-bad-tiles function
; It aids the latter by getting the first node in the path parameter for it as a parameter.
; This action cannot be easily done anywhere else, so having this intermediary method is convenient for passing a node alone.
; params: a path (path) and a goal state (goal-state) to be passed to the count-bad-tiles function in accordance with its own parameters.
; return: the result of count-bad-tiles, a metric concerning how many tiles do not match up between the current state of the path and the goal state.
(define bound-misalignment
  (lambda (path goal-state)
    (+ (length path) (count-bad-tiles (car path) goal-state))))

; The following method performs a depth-first search to solve the Slide Puzzle problem. It uses the GeneralSearch algorithm to do so.
; It uses its parameters to pre-compose functions that check against the goal-state; compose-gs-predicate is an example of a helper method that does this.
; params: an initial state for the program (start-state), a boolean to check whether a given state equals the desired goal state (goal-state?),
; a goal state for the program (goal-state), a function which calculates and returns the ensuing states based on a given state (successor),
; and a function which can read and print paths more visually (path-printer).
; return: a printed notice about whether the search was successful, what the number of nodes explored was (if successful), and what a correct path is from the start to the goal (if successful).
(define slide-puzzle-dfs
  (lambda (start-state goal-state? goal-state successor path-printer)
    (GeneralSearch (list (list start-state)) '() (compose-gs-predicate goal-state? goal-state) successor 0 append path-printer)))

; The following method performs a breadth-first search to solve the Slide Puzzle problem.  It uses the GeneralSearch algorithm to do so.
; It uses its parameters to pre-compose functions that check against the goal-state; compose-gs-predicate is an example of a helper method that does this.
; params: an initial state for the program (start-state), a boolean to check whether a given state equals the desired goal state (goal-state?),
; a goal state for the program (goal-state), a function which calculates and returns the ensuing states based on a given state (successor),
; and a function which can read and print paths more visually (path-printer).
; return: a printed notice about whether the search was successful, what the number of nodes explored was (if successful), and what a correct path is from the start to the goal (if successful).
(define slide-puzzle-bfs
  (lambda (start-state goal-state? goal-state successor path-printer)
    (GeneralSearch (list (list start-state)) '() (compose-gs-predicate goal-state? goal-state) successor 0 tail-append path-printer)))

; The following method performs a best-first search to solve the Slide Puzzle problem. It uses the GeneralSearch algorithm to do so.
; It uses its parameters to pre-compose functions that check against the goal-state; compose-gs-predicate and compose-overall-sort are examples of helper methods that do this.
; params: an initial state for the program (start-state), a boolean to check whether a given state equals the desired goal state (goal-state?),
; a goal state for the program (goal-state), a function which calculates and returns the ensuing states based on a given state (successor),
; and a function which can read and print paths more visually (path-printer).
; return: a printed notice about whether the search was successful, what the number of nodes explored was (if successful), and what a correct path is from the start to the goal (if successful).
(define slide-puzzle-bestfs
  (lambda (start-state goal-state? goal-state successor path-printer)
    (GeneralSearch (list (list start-state)) '() (compose-gs-predicate goal-state? goal-state) successor 0 (compose-overall-sort bestfs-sort goal-state) path-printer)))

; The following method performs a branch-and-bound+ search to solve the Slide Puzzle problem. It uses the GeneralSearch algorithm to do so.
; It uses its parameters to pre-compose functions that check against the goal-state; compose-gs-predicate and compose-overall-sort are examples of helper methods that do this.
; params: an initial state for the program (start-state), a boolean to check whether a given state equals the desired goal state (goal-state?),
; a goal state for the program (goal-state), a function which calculates and returns the ensuing states based on a given state (successor),
; and a function which can read and print paths more visually (path-printer).
; return: a printed notice about whether the search was successful, what the number of nodes explored was (if successful), and what a correct path is from the start to the goal (if successful).
(define slide-puzzle-bnb+
  (lambda (start-state goal-state? goal-state successor path-printer)
    (GeneralSearch (list (list start-state)) '() (compose-gs-predicate goal-state? goal-state) successor 0 (compose-overall-sort bnb+-sort goal-state) path-printer)))

; The following automatically gives arguments to slide-puzzle-dfs to run it. Defer to slide-puzzle-dfs for documentation concerning what the function does.
(define auto-dfs
  (lambda (start-state goal-state)
    (slide-puzzle-dfs start-state goal-state? goal-state successor path-printer)))

; The following automatically gives arguments to slide-puzzle-bfs to run it. Defer to slide-puzzle-bfs for documentation concerning what the function does.
(define auto-bfs
  (lambda (start-state goal-state)
    (slide-puzzle-bfs start-state goal-state? goal-state successor path-printer)))

; The following automatically gives arguments to slide-puzzle-bestfs to run it. Defer to slide-puzzle-bestfs for documentation concerning what the function does.
(define auto-bestfs
  (lambda (start-state goal-state)
    (slide-puzzle-bestfs start-state goal-state? goal-state successor path-printer)))

; The following automatically gives arguments to slide-puzzle-bnb+ to run it. Defer to slide-puzzle-bnb+ for documentation concerning what the function does.
(define auto-bnb+
  (lambda (start-state goal-state)
    (slide-puzzle-bnb+ start-state goal-state? goal-state successor path-printer)))

; The following are predefined constants used for testing. They are not integral to the code in any way and are present merely for convenience.
(define start-state '(1 2 3 8 4 5 0 7 6))
(define start-state-2 '(2 3 8 1 7 4 6 5 0))
(define start-state-3 '(8 1 0 7 5 3 6 4 2))
(define start-state-4 '(1 3 4 8 2 5 7 6 0))
(define start-state-5 '(8 1 2 0 4 3 7 6 5))
(define start-state-6 '(2 0 3 1 6 4 8 7 5))
(define start-state-7 '(1 6 2 8 0 3 7 5 4))
(define start-state-8 '(0 1 3 6 2 7 8 5 4))
(define start-state-9 '(2 3 4 1 8 5 7 6 0))
(define start-state-10 '(1 2 3 4 5 6 7 8 10 11 12 0 9 13 14 15))

(define goal-state '(1 2 3 8 0 4 7 6 5))
(define goal-state-2 '(1 2 3 4 5 6 7 8 0))
(define goal-state-3 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0))

; The following is a method that runs every kind of sort on a given initial state (start-state) and final state (goal-state).
; params: an initial state (start-state) and a final state (goal-state) for each search algorithm to be performed on.
; return: print-outs of the results of every search.
(define run-all-sorts
  (lambda (start-state goal-state)
    (display "Depth-First Search: ")
    (newline)
    (auto-dfs start-state goal-state)
    (newline)
    (newline)
    (display "Breadth-First Search: ")
    (newline)
    (auto-bfs start-state goal-state)
    (newline)
    (newline)
    (display "Best-First Search: ")
    (newline)
    (auto-bestfs start-state goal-state)
    (newline)
    (newline)
    (display "Branch-and-Bound+ Search: ")
    (newline)
    (auto-bnb+ start-state goal-state)))

; Executed Code:
(run-all-sorts start-state goal-state)