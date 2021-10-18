#lang racket

;Environment Setup with a local state number so that multiple instances will not interact with each other.
;Input: Deposit (Money play can deposit to start the game)
;Usage: (define P1(a-game 20))
;Where 20 can be replaced with any integer, it is only an example.
(define (a-game deposit)
  (let ((number deposit))
    (display "Game Player, you decided to go with number: ") (display number)
    ;If the player deposits money between 2 and 30, they are allowed to play the game. Otherwise it throws an error message.
    (cond
      [(or (< number 2) (> number 30)) (display "\nYou cannot deposit this amount, the minimum is £2 and the maximum is £30")]
      [else
       (display "\nGreat Choice!")

      (display "\n-------RULES-------\nIf Player wins, then 1 point is scored which equates to £1 added onto your number and deduct £2 from the Machines account.\nIf Player loses, then he will lose 2 points, which equates to losing £2 from its account and add £1 to the Machines account.\nThe game will end when the player runs out of credit.\n-------------------")

      ;the-game-number takes an input that refers to one of the four predefined procedures.
      ;Input: request (string)
      (define (the-game-number)
        ;Takes a string input and returns the procedure
        (lambda (request)

          ;I have commented the following which I believe is a better randomnum method as it eliminated bad coding practices such as overriding schemes random procedure with a localstate
          ;This method is also better because not only does it allow people to pass random as a parameter to allow randomnum to takeover and generate a randomnumber, it also allows the user to input a random number
          ;(define (randomnum number)
          ;(cond ((eq? number random) (set! number (random 2 51)))) ;Here we check if number is equivalent to schemes random procedure, if it is equivalent we will overwrite number with schemes random procedure with parameters 2 and 51
          ;If number is equal to schemes random procedure then the program will continue using whatever number the user provided. This is a more modular / universal design and eliminated possible confusions or errors.
          ;Rahul said that I should comment my better method, but use the original because multiple tutors will be marking this.


          ;randomnum
          ;A random number is generated, and evaluated against the players number.
          ;If the players number is larger, the player wins
          ;If the players number is lower or equal to, the machine wins.
          ;Input: random
          ;Usage: ((P1 'randomnum) random)
          (define (randomnum random) ;unfortunately in scheme, if we use the word random at all without doing 'random instead, it automatically refers to schemes random procedure.
            (cond
              ((eq? random random) (set! random (random 2 51)))) ;Here we use this to check the random parameter passed is equal to the random procedure, we also use this to initialise random to allow us to alter the localstate random to generate a number between 2 and 50
            (display "\nThe machines random number is: ") (display random)
            (display "\nPlayer number is: ") (display number)

            (cond
              [(or (< number random) (= number random)) ;Or statement to check if the user number is the same or less than the random generated number
               (display "\nYour number is less than or equal to the machines random number.\nYou have lost, £2 is deducted from your account.")]
              [else
               (display "\nYou have won! Game machine will add £1 to your account.")]
            )
          )
        
          ;decreasemoney
          ;Decreases the players money if the player has lost
          ;Usage: ((P1 'decreasemoney))
          (define (decreasemoney)
            (display "\nPlayer, you previously had: £") (display number)
            (set! number (- number 2))
            (display "\nAs you have lost, your balance has been decreased by £2.\nYour new balance is: £") (display number)

            (if (> number 1)
                (display "\nYou still have enough credit to play!")
                (display "\nSorry, you are out of credit, which you can't continue to play. To continue playing, you need to top-up. See you soon!!!")
            )
          )

          ;topup
          ;Player inputs more money to continue playing
          (define (topup balance)
            (let ((t balance)) ;We do this to achieve mutual exclusivity between objects P1 and P2
              (display "\nPlayer you have topped up: £") (display t)
              (cond
                [(or (< t 2) (> t 30)) (display "\n\nYou cannot deposit this amount, the minimum is £2 and the maximum is £30")]
                [else
                 (set! number t)
                 (display "\nGreat, you can play now!")]
              )
            )
          )

          ;increasemoney
          ;Increases the players balance if they have won the game
          (define (increasemoney)
            (display "\nPlayer, you previously had: £") (display number)
            (set! number (+ number 1))
            (display "\nYou've scored 1 point and have been awarded £1 by the Machine!")
            (display "\nPlayer, you now have: £") (display number)
          )

          ;Handles the request the user makes and calls appropriate procedures
          ;eq is to check equivalence
          (define (caller)
            (cond
              ((eq? request 'increasemoney) increasemoney)
              ((eq? request 'topup) topup)
              ((eq? request 'decreasemoney) decreasemoney)
              ((eq? request 'randomnum) randomnum)
              (else (error "Unknown Request:" request))))
          
        (caller)
          )
        )
      (the-game-number)
      ]
      )
    )
  )

         
;game_machine_amount contains the balance of the machine     
(define game_machine_amount (random 2 31))

;game_machine_decrement
;Decrements the game machines balance if it loses
(define (game_machine_decrement)
  (display "\nGame Machine, you had: £") (display game_machine_amount)
  (set! game_machine_amount (- game_machine_amount 2))
  (display "\nGame Machine, you now have: £") (display game_machine_amount)

  (cond
    [(or (< game_machine_amount 1) (= game_machine_amount 1)) (display "\nGame Machine, there isn't enough money in the machine for a game to be played.")]
    [else
     (display "\nGame Machine, there is enough money in the machine for a game to be played.")]
   )
)

;game_machine_increment
;Increment the game machines balance if it wins
(define (game_machine_increment)
  (display "\nGame Machine, you had: £") (display game_machine_amount)
  (set! game_machine_amount (+ game_machine_amount 1))
  (display "\nGame Machine, you now have: £") (display game_machine_amount)
 )



