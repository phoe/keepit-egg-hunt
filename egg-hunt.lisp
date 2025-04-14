;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Keepit Egg Hunt
;;;;
;;;; To run, compile and load this file and call (keepit-egg-hunt:boot).
;;;;
;;;; © Keepit A/S 2025. All rights reserved.
;;;; Permission is granted for reading the code.
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package

#-sbcl
(error "This Egg Hunt is built for SBCL.")

(defpackage #:keepit-egg-hunt
  (:use #:cl)
  (:export #:boot #:start #:build)
  (:export
   #:scary-egg #:*i-postordered-a-whole-box-of-eggs* #:whipped-eggs
   #:eggeneric-function #:+a-very-curious-integger+ #:call-with-egg-context
   #:*egg-hunt* #:execute-the-egg-dispensing-machine #:egg-in-rot13-is-rtt
   #:retrieve-egg-with-authentication #:no-easter-eggs-here)
  (:export #:*user* #:*password* #:check-credentials
           #:*dispensing-machine-broken-p* #:it-is-easter
           #:make-chicken #:chicken-combination)
  (:export #:ostrich #:ostrich-egg #:dead-ostrich #:use-bird-time-machine)
  (:export #:egg-vault-key #:brokenp #:enter-the-egg-vault
           #:macroeggspand-1 #:my-favorite-egg-function
           #:*my-favorite-egg-source*
           #:not-what-it-seems #:impatience-is-a-virtue
           #:locked-and-unopenable #:call-with-temporary-packegge
           #:*eyes-wide-shut* #:eyes-wide-shut #:*poof-and-its-gone*))

(in-package #:keepit-egg-hunt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STR macro
;;;
;;; Obfuscate literal strings by turning them into lambdas filled with
;;; individual WRITE-CHAR calls.
;;; Used to make literal strings in disassembly invisible.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %str (string env)
    (when (consp string)
      (setf string (macroexpand string env)))
    (let ((gensym (gensym)))
      `(flet ((,gensym ()
                (with-output-to-string (stream)
                  ,@(loop for char across string
                          collect `(write-char ,char stream)))))
         (declare (inline ,gensym))
         #',gensym))))

(defmacro str (string &environment env)
  (%str string env))

(defmacro strs (&rest strings &environment env)
  `(list ,@(mapcar (lambda (x) (%str x env)) strings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Puzzle framework

(defvar *puzzles* '())

(defmacro define-puzzle (symbol &rest xs)
  (dolist (x xs)
    (check-type x string))
  `(pushnew (list ',symbol
                   ,@(loop for s in xs
                           collect `(lambda (x)
                                      (string= x (funcall (str ,s))))))
            *puzzles* :test #'equal :key #'car))

(defvar *solved* '())

(defun verify (&optional string)
  "When called with no arguments, print the list of all remaining eggs.

When called with a string, also verify if a string is a valid egg."
  (check-type string (or null string))
  (when string
    (if (member string *solved* :test #'string=)
        (format t ";; This egg is already verified.~%")
        (loop named outer for puzzle in *puzzles* do
          (loop with (name . functions) = puzzle
                for cons on functions
                for function = (car cons)
                when (and function (funcall function string))
                  do (format t ";; Bingo! ~A has one egg less.~%" name)
                     (pushnew string *solved* :test #'string=)
                     (setf (car cons) nil)
                     (return-from outer))
              finally (format t ";; Nope, that's not a correct egg.~%")))
    (format t ";;~%"))
  (format t ";; ~D eggs remain:~%;;~%"
          (length (remove nil (apply #'append (mapcar #'cdr *puzzles*)))))
  (loop with n = 0
        for (name . functions)
          in (sort (copy-list *puzzles*) #'string<
                   :key (lambda (x) (princ-to-string (car x))))
        unless (null (remove nil functions))
          do (incf n)
             (format t ";; ~40,,,'.A : ~D eggs~%"
                     (concatenate 'string (symbol-name name) (string #\Space))
                     (length (remove nil functions)))
        finally (when (= n 0)
                  (format t (funcall (str ";; Congratulations. You're all done!
;;
;; Feel free to check if https://careers.keepit.com/ has any Lisp jobs posted.
;;
;; Also, send this egg hunt to your Lisper pals and ask them how many eggs
;; they manage to find here!
;;
;; Feedback: {mhr,wga}@keepit.com
"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Boot

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-bsd-sockets)
  (require 'sb-introspect)
  (require 'sb-posix)
  (require 'sb-cltl2)
  (require 'asdf))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:restrict-compiler-policy 'debug))

(declaim (optimize (debug 1)))

(defun boot ()
  "Prepare the egg hunt environment."
  (let ((user (car (last (pathname-directory (user-homedir-pathname))))))
    (setf (symbol-value '*user*) user
          (symbol-value '*password*) "hunter2"))
  (asdf:initialize-source-registry)
  (format t ";;
;; Keepit Easter Egg Hunt loaded.
;;
;; To begin, go into package KEEPIT-EGG-HUNT, call (START),
;; and follow the white rabbit.
")
  (in-package #:keepit-egg-hunt))

(defun start ()
  (format t ";; This is an egg hunt/capture the flag style challenge.
;;
;; You need to find eggs that we planted all over the Lisp image.
;; Eggs have the shape of strings written in uppercase, without
;; any other characters in between. THISISWHATANEXAMPLEEGGLOOKSLIKE
;;
;; Whenever you are lost, call #'VERIFY to show the remaining eggs.
;; Whenever you find an egg, call #'VERIFY on it to verify it.
;;
;; Be creative. Describe. Debug. Disassemble. Rebind. Redefine.
;; In the worst case, remember your *SOLVED* and rerun the challenge.
;;
;; Symbols that are a part of the puzzle framework are:
;;   BOOT BUILD START VERIFY *SOLVED*
;; Inside, there are only spoilers and disappointment. Not eggs.
;; Please don't spend your time there.
;;
;; Take notes of how you found the eggs. We'll want to hear them.
;;
;; All bets are off, any creative means of getting the eggs are good.
;; This is not a speedrun. You are not required to find all of them.
;; If anything, you are encouraged to have fun.
;;
;; -- {mhr,wga}@ keepit.com
")
  'good-luck-have-fun)

(defun build (&optional (pathname #p"/tmp/keepit-egg-hunt"))
  (sb-ext:save-lisp-and-die
   pathname
   :toplevel (lambda ()
               (keepit-egg-hunt:boot)
               (let ((sb-impl::*posix-argv* '("--no-sysinit" "--no-userinit")))
                 (sb-impl::toplevel-init)))
   :executable t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Puzzles
;;;
;;;  _    _  ___  ______ _   _ _____ _   _ _____ _
;;; | |  | |/ _ \ | ___ \ \ | |_   _| \ | |  __ \ |
;;; | |  | / /_\ \| |_/ /  \| | | | |  \| | |  \/ |
;;; | |/\| |  _  ||    /| . ` | | | | . ` | | __| |
;;; \  /\  / | | || |\ \| |\  |_| |_| |\  | |_\ \_|
;;;  \/  \/\_| |_/\_| \_\_| \_/\___/\_| \_/\____(_)
;;;
;;; There are implementation spoilers beneath this line.
;;;
;;; If you want to have fun solving them, don't read further.
;;;
;;;  _    _  ___  ______ _   _ _____ _   _ _____ _
;;; | |  | |/ _ \ | ___ \ \ | |_   _| \ | |  __ \ |
;;; | |  | / /_\ \| |_/ /  \| | | | |  \| | |  \/ |
;;; | |/\| |  _  ||    /| . ` | | | | . ` | | __| |
;;; \  /\  / | | || |\ \| |\  |_| |_| |\  | |_\ \_|
;;;  \/  \/\_| |_/\_| \_\_| \_/\___/\_| \_/\____(_)
;;;
;;; You have been warned.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *PACKAGE*

(setf (documentation *package* 't) "\"YOUCANEXPECTEGGSINALLSORTSOFPLACES\"")

(define-puzzle *package*
  "YOUCANEXPECTEGGSINALLSORTSOFPLACES")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NO-EASTER-EGGS-HERE

(let ((x (strs "There are no Easter Eggs in this function."
               "There really are no Easter Eggs in this function."
               "Didn't I already tell you that there are no Easter Eggs?"
               "Stop it!"
               "Give up! We cannot proceed like this."
               "Were you sent by the Common Lisp Mafia!?"
               "All right. Here you go, you insensitive clod."
               "Let's give you the secret that you so badly want."
               "Enjoy it."
               "Yes. Enjoy it while it lasts."
               "Hope you don't miss it once it finally appears."
               "Good luck."
               "SOMETIMESDOINGTHESAMETHINGOVERANDOVERISNOTMADNESS")))
  (nconc (last x) (last x))
  (defun no-easter-eggs-here ()
    "Sometimes, you just need to be a little bit insistent."
    (funcall (pop x))))

(define-puzzle no-easter-eggs-here
  "SOMETIMESDOINGTHESAMETHINGOVERANDOVERISNOTMADNESS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AUTHENTICATION

(defvar *user*)
(defvar *password*)

(defun check-credentials (user password)
  (unless (stringp user)
    (error (funcall (str "User must be a string."))))
  (let ((a0 (char-code (char user 0)))
        (a1 (char-code (char user 1)))
        (a2 (char-code (char user 2))))
    (unless (or (and (= a0 109) (= a1 104) (= a2 114)) ;; mhr
                (and (= a0 119) (= a1 103) (= a2 97))) ;; wga
      (error (funcall (str "User does not have egg hunt admin rights.")))))
  (unless (stringp password)
    (error (funcall (str "Password must be a string."))))
  (when (string= password (funcall (str "hunter2")))
    (error (funcall (str "This password is too secure. ~
                          Use a less secure one."))))
  (unless (= (length password) (length (remove-duplicates password)))
    (error (funcall (str "Password must not contain duplicate characters."))))
  (unless (= 8 (length password))
    (error (funcall (str "Password must be between 8 and ~
                          8 characters long."))))
  (flet ((frob (char)
           (find char (funcall (str "1234567890qwertyuiopasdfghjklzxcvbnm")))))
    (unless (every #'frob password)
      (error (funcall (str "Password must contain only numbers and ~
                            lowercase Latin letters.")))))
  (unless (char= #\1 (char password 0))
    (error (funcall (str "Password must begin with the substring \"1\"."))))
  (unless (and (char= #\x (char password 6))
               (char= #\y (char password 7)))
    (error (funcall (str "Password must end with the substring \"xy\"."))))
  (unless (search (funcall (str "456")) password)
    (error (funcall (str "Password must contain the substring \"456\"."))))
  (unless (find #\a password)
    (error (funcall (str "Password must contain the substring \"a\"."))))
  (unless (loop for i below (1- (length password))
                always (char< (char password i) (char password (1+ i))))
    (error (funcall (str "Password must be ordered lexicographically."))))
  (unless (string= password (funcall (str "1456afxy")))
    (error (funcall (str "Incorrect password. ~
                          (Seriously, even after all those hints!? ~
                           Press F to pay respects.)"))
           user)))

(defun retrieve-egg-with-authentication (&optional (user *user*)
                                           (password *password*))
  "\"Leeloo Dallas, multipass.\"
    -- Leeloo, The Fifth Element, 1997"
  (check-credentials user password)
  (let ((a0 (char-code (char user 0)))
        (a1 (char-code (char user 1)))
        (a2 (char-code (char user 2))))
    (funcall (if (and (or (and (= a0 109) (= a1 104) (= a2 114))
                          (and (= a0 119) (= a1 103) (= a2 97)))
                      (string= password (funcall (str "1456afxy"))))
                 (str "ITSIMPOSSIBLEHOWDIDYOUDOTHAT")
                 (str "ATRUEHACKERMANBYSPIRITAREYOUNOT")))))

(define-puzzle retrieve-egg-with-authentication
  "ITSIMPOSSIBLEHOWDIDYOUDOTHAT"
  "ATRUEHACKERMANBYSPIRITAREYOUNOT")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EGG-IN-ROT13-IS-RTT

(defun egg-in-rot13-is-rtt ()
  "Va pnfr bs qbhog, ebgngr ol guvegrra."
  (macrolet
      ((rot (string offset &optional (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
         (with-output-to-string (stream)
           (loop for char across string
                 for position = (position char alphabet :test #'char-equal)
                 if (null position)
                   do (write-char char stream)
                 else do
                   (let* ((new-position (mod (+ position offset)
                                             (length alphabet)))
                          (new-char (char alphabet new-position))
                          (new-char (if (upper-case-p char)
                                        new-char
                                        (char-downcase new-char))))
                     (write-char new-char stream))))))
    (funcall (str (rot "THISONEISSIMPLEGOGRABSOMETHINGHARDER" 13)))))

(define-puzzle egg-in-rot13-is-rtt
  "THISONEISSIMPLEGOGRABSOMETHINGHARDER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OOPS-WE-FORGOT-TO-OBFUSCATE-THIS-ONE

(defun oops-we-forgot-to-obfuscate-this-one ()
  "This function has been \"accidentally\" compiled with high debug."
  (declare (optimize debug))
  (format t ";; Sorry, this function is not allowed to
;; not return the egg stored inside.~%")
  (when (funcall (load-time-value (lambda ())))
    "YOUARELUCKYDISASSEMBLYWORKSONTHISONE"))

(define-puzzle oops-we-forgot-to-obfuscate-this-one
  "YOUARELUCKYDISASSEMBLYWORKSONTHISONE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CATCH-ME-IF-YOU-CAN

(defun catch-me-if-you-can ()
  "No time to explain. You need to catch it somewhere!"
  (throw :somewhere (funcall (str "GOTTAESTABLISHTHEPROPERCATCHFORTHAT"))))

(define-puzzle catch-me-if-you-can
  "GOTTAESTABLISHTHEPROPERCATCHFORTHAT")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RUN-THE-EGG-DISPENSING-MACHINE

(defvar *dispensing-machine-broken-p* t)

(defun run-the-egg-dispensing-machine ()
  "Sometimes, you might want to fix a machine. Other times, you won't need to."
  (declare (optimize debug))
  (if (null *dispensing-machine-broken-p*)
      (funcall (str "FIXINGBUGSGIVESYOULOTSOFSTREETCRED"))
      (let ((x (funcall (str "YOUCANPEEKINTOTHEMACHINEASITISRUNNING"))))
        (when (funcall (load-time-value (lambda ())))
          (return-from run-the-egg-dispensing-machine x))
        (loop
          (with-simple-restart (continue "Try to service the machine.")
            (error "The egg dispensing machine is stuck and cannot proceed."))
          (with-simple-restart (continue "Kick the machine.")
            (error "The egg dispensing machine cannot be user-serviced.~@
                    For service and support, please call +1-800-FIXMYEGGS."))
          (with-simple-restart (continue "Hack the machine.")
            (error "Please do not destroy vital Keepit machinery."))
          (with-simple-restart (continue "Give up.")
            (error "This egg dispensing machine cannot be hacked.~@
                    You are not allowed to hack this machine.~@
                    Hacking this machine in general is not permitted.~@
                    This incident will be reported to Keepit Security Team."))
          (with-simple-restart (continue "Start from the beginning.")
            (error "Hey, that's not the Easter spirit! Don't give up!"))))))

(define-puzzle run-the-egg-dispensing-machine
  "FIXINGBUGSGIVESYOULOTSOFSTREETCRED"
  "YOUCANPEEKINTOTHEMACHINEASITISRUNNING")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CALL-WITH-EGG-CONTEXT

(define-condition it-is-easter () ())

(defvar *egg-hunt* "THISONEISAFREEBIEDONTCHATHINK"
  "There's an egg in plain sight. There might be more hidden, though.")

(define-puzzle *egg-hunt*
  "THISONEISAFREEBIEDONTCHATHINK")

(defun call-with-egg-context (function)
  "There are different eggs hidden here, all of them very dynamically."
  (flet ((handler (c)
           (declare (ignore c))
           (return-from call-with-egg-context
             (funcall (str "THEEASTERBUNNYISGONNAKEEPHISJOB"))))
         (restart ()
           (return-from call-with-egg-context
             (funcall (str "WHATWASFIRSTTHEEGGORTHECHICKEN"))))
         (report (stream)
           (princ (funcall (str "Make a chicken instead.")) stream)))
    (restart-bind ((make-chicken #'restart :report-function #'report))
      (handler-bind ((it-is-easter #'handler))
        (let ((*egg-hunt* (funcall (str "THESPRINGISTRULYINFULLFORCE"))))
          (funcall function))))))

(define-puzzle call-with-egg-context
  "THEEASTERBUNNYISGONNAKEEPHISJOB"
  "WHATWASFIRSTTHEEGGORTHECHICKEN"
  "THESPRINGISTRULYINFULLFORCE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A-VERY-CURIOUS-INTEGER

(defconstant +a-very-curious-integger+
  (parse-integer "THEYWEREDOINGHIGHRADIXMATHINLISP" :radix 36)
  "If only we had thirty six digits to use.")

(define-puzzle +a-very-curious-integger+
  "THEYWEREDOINGHIGHRADIXMATHINLISP")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EGGENERIC-FUNCTION

;; TODO more eggs in method combinations

(define-method-combination chicken-combination ()
  ((methods *))
  (flet ((first-qualifier (x) (first (method-qualifiers x))))
    (let* ((qualified-methods (remove nil methods
                                      :key #'method-qualifiers))
           (sorted-methods (sort (copy-list qualified-methods)
                                 #'string<
                                 :key #'first-qualifier))
           (unqualified-method (find nil methods :key #'method-qualifiers)))
      `(progn ,@(loop for method in sorted-methods
                      collect `(call-method ,method))
              ,@(when unqualified-method
                  `((call-method ,unqualified-method)))))))

(defgeneric eggeneric-function (x)
  (:method-combination chicken-combination)
  (:documentation
   "#'SORT my qualifiers by #'STRING<, CONCATENATE 'STRING them, and ask me
if you got it right!"))

(defmethod eggeneric-function chicken (x))

(defmethod eggeneric-function rooster (x))

(defmethod eggeneric-function hen (x))

(defmethod eggeneric-function coop (x))

(defmethod eggeneric-function clucking (x))

(defmethod eggeneric-function ((x string))
  (equal x (funcall (str "CHICKENCLUCKINGCOOPHENROOSTER"))))

(define-puzzle eggeneric-function
  "CHICKENCLUCKINGCOOPHENROOSTER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WHIPPED-EGGS

(defun whipped-eggs ()
  "Spiral. Counterclockwise. Outside-in."
  (format t "~A" (funcall (str "HONERA
YTOYRS
PGU!OE
NOODFB
OTICVI"))))

(define-puzzle whipped-eggs
  "HYPNOTICVIBESARENOTGOODFORYOU!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I-POSTORDERED-A-WHOLE-BOX-OF-EGGS

#+(or)
(defun split-string (string)
  (case (length string)
    (0 (error "oops"))
    (1 string)
    (2 (cons (subseq string 1)
             (subseq string 0 1)))
    (t (let ((n (1+ (random (- (length string) 1)))))
         (cons (split-string (subseq string n))
               (split-string (subseq string 0 n)))))))

(defvar *i-postordered-a-whole-box-of-eggs*
  '(("D" . "N") ((("U" . "O") ("R" . "A") . "R") "D" . "C") (("D" . "N") . "A")
    "R" "A" (("C" "H" . "C") ("T" . "I") ("W" . "S") . "T") ("S" . "U") . "J")
  "Still, better than doing preorders.")

(define-puzzle *i-postordered-a-whole-box-of-eggs*
  "JUSTSWITCHCARANDCDRAROUND")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCARY-EGG

(defun scary-egg ()
  "This one could actually get me fired from Keepit if our Infosec found out."
  (cerror "Continue anyway."
          "Executing this function may expose your computer to various risks.")
  (let* ((string (funcall
                  (str "RUNNINGUNTRUSTEDSOFTWAREONYOURMACHINEISNOTAGOODIDEA
")))
         (pathname #p"/tmp/" ;; (uiop:temporary-directory)
                   )
         (pathname (merge-pathnames "scary-egg.txt" pathname)))
    (with-open-file (stream pathname :direction :output
                                     :if-exists :supersede)
      (write-sequence (princ-to-string string) stream))
    (format t (funcall (str ";; Working. Please wait...")))
    (loop repeat 200 do (sleep (* (random 0.6)
                                  (random 0.6)
                                  (random 2.0)))
                        (princ "."))
    (format t (funcall (str "~&;; Done.
;; Thank you for giving us your credit card information and for
;; sending all of your browser cookies to online hackers.
;; Your files are now encrypted and it's time to restore from backup.
;; For further instructions, check the instructions available at
;; ~A."))
            pathname)))

(define-puzzle scary-egg
  "RUNNINGUNTRUSTEDSOFTWAREONYOURMACHINEISNOTAGOODIDEA")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPLETELY-ORDINARY-FUNCTION

(defun completely-ordinary-function ()
  (format t ";; This is not the function you're looking for.~%")
  (format t ";; (Have you tried looking elsewhere?)")
  'jedi-mind-trick)

(defpackage #:totally-not-secret-egg-package
  (:use)
  (:export #:completely-ordinary-function)
  (:documentation "Perhaps it is actually somewhat secret."))

(defun totally-not-secret-egg-package:completely-ordinary-function ()
  (if (eq *package* (find-package '#:totally-not-secret-egg-package))
      (funcall (str "THINKOUTSIDETHEBOXLOOKOUTSIDETHEPACKAGE"))
      (format t ";; Oh snap, it's an outsider! Quick, hide our egg...")))

(define-puzzle completely-ordinary-function
  "THINKOUTSIDETHEBOXLOOKOUTSIDETHEPACKAGE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OSTRICH

(defclass ostrich () ()
  (:documentation "A very, very, very big chicken."))

(defclass dead-ostrich (ostrich) ()
  (:documentation "A very, very, very big dead chicken."))

(defclass ostrich-egg (ostrich) ()
  (:documentation "A very, very, very big chicken egg."))

(defmethod initialize-instance :before ((object ostrich-egg)
                                        &key)
  (error "There is nowhere to get an ostrich egg from."))

(defmethod initialize-instance :before ((object dead-ostrich)
                                        &key)
  (error "There is no use beating a dead ostrich."))

(defmethod print-object ((object ostrich-egg) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (funcall (str "YOUWOULDNTDOWNLOADANOSTRICHEGG")))))

(defgeneric use-bird-time-machine (bird direction)
  (:documentation
   "A function for time travel. Currently specialized only on birds.")
  (:method ((bird ostrich-egg) (direction (eql :backwards)))
    nil)
  (:method ((bird ostrich-egg) (direction (eql :forwards)))
    (change-class bird 'ostrich)
    t)
  (:method ((bird ostrich) (direction (eql :backwards)))
    (change-class bird 'ostrich-egg)
    t)
  (:method ((bird ostrich) (direction (eql :forwards)))
    (change-class bird 'dead-ostrich)
    t)
  (:method ((bird dead-ostrich) (direction (eql :backwards)))
    (change-class bird 'ostrich)
    t)
  (:method ((bird dead-ostrich) (direction (eql :forwards)))
    nil))

(define-puzzle use-bird-time-machine
  "YOUWOULDNTDOWNLOADANOSTRICHEGG")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ENTER-THE-EGG-VAULT

(defclass egg-vault-key ()
  ((%brokenp :initform t
             :reader brokenp))
  (:documentation "THEREISNOVAULTKEYDONOTLOOKFORIT"))

(defmethod initialize-instance :before ((object egg-vault-key)
                                        &key (brokenp nil brokenpp))
  (declare (ignore brokenp))
  (when brokenpp
    (error "Creating unbroken vault keys is NOT allowed!!111
Please call +1-800-VAULTKEYS to order certified, working keys.
(Prices starting from $399.99 per key.)")))

(defmethod (setf brokenp) (newval (object egg-vault-key))
  (if (null newval)
      (error "Fixing egg vault keys is NOT allowed!!!!11
Please call +1-800-VAULTKEYS to order certified, working keys.
(Prices starting from $399.99 per key.)")
      (call-next-method)))

(defgeneric enter-the-egg-vault (key)
  (:documentation
   "Allows you to enter the egg vault, if you have a working key.")
  (:method ((key egg-vault-key))
    (if (brokenp key)
        (funcall (str "REMOVINGOBSTACLESISGOODEVENIFTHEYAREMETHODS"))
        (funcall (str "THEVAULTISFULLOFEGGSBUTYOUMAYONLYHAVEONE"))))
  (:method :around ((key egg-vault-key))
    (if (brokenp key)
        (error "The key is broken, you CANNOT use it to enter the Egg Vault!!1
Please call +1-800-VAULTKEYS to order certified, working keys.
(Prices starting from $399.99 per key.)")
        (call-next-method))))

(define-puzzle enter-the-egg-vault
  "REMOVINGOBSTACLESISGOODEVENIFTHEYAREMETHODS"
  "THEVAULTISFULLOFEGGSBUTYOUMAYONLYHAVEONE"
  "THEREISNOVAULTKEYDONOTLOOKFORIT")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EGGSTREMELY-FASCINATING-TYPE

(deftype eggstremely-fascinating-type ()
  `(member "THISEGGHUNTISBECOMINGWEIRDERBYTHESECOND"))

(defun chegg-type (x)
  "Checks if X is of a very, very, very peculiar type."
  (assert (typep x 'eggstremely-fascinating-type) (x)
          "The value
  ~S
is not of type
  ~S" x 'eggstremely-fascinating-type))

(define-puzzle eggstremely-fascinating-type
  "THISEGGHUNTISBECOMINGWEIRDERBYTHESECOND")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MY-FAVORITE-EGG-FUNCTION

(defparameter *my-favorite-egg-source*
  `(defun my-favorite-egg-function ()
     (let (error "This source code is scrambled by EggForce® DRM.")
       ((s6 "I") (s35 "E") (s7 "M") (s30 "C") (s21 "G") (s13 "M")
        (s11 "O") (s33 "O") (s2 "O") (s3 "M") (s28 "U") (s31 "E")
        (s23 "T") (error "This source code is scrambled by EggForce® DRM.")
        (s32 "C") (s10 "Y") (s8 "E") (s12 "U") (s25 "E")
        (s15 "S") (s16 "T") (s14 "U") (s4 "E") (s9 "S") (s34 "D")
        (s18 "H") (s17 "C") (s20 "N") (s26 "S") (s1 "S") (s5 "T")
        (error "This source code is scrambled by EggForce® DRM.")
        (s29 "R") (s24 "H") (s19 "A") (s22 "E") (s27 "O"))
       (concatenate
        'string s12 s20 s28 s6 s21 s22 s9
        (error "This source code is scrambled by EggForce® DRM.")
        s1 s13 s19 s2 s7 s11 s33 s29 s17 s25 s26 s16
        s23 (error "This source code is scrambled by EggForce® DRM.") s32
        s34 s10 s14 s24 s35 s30 s18 s15 s27 s31
        (error "This source code is scrambled by EggForce® DRM.")
        s8 s4 s5 s3)))
  "This source code is scrambled by EggForce® DRM.
(Unscramble before use.)")

(defun my-favorite-egg-function ()
  "Recompilable egg function, removed from this Lisp program.
Check the corresponding source code (protected with EggForce® DRM)
to prepare the function for being used."
  (error "This function has been removed to prevent egg leak."))

(define-puzzle my-favorite-egg-function
  "SOMETIMESYOUMUSTCHANGETHESOURCECODE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EGGSTREMELY-FASCINATING-TYPE

(defun macroeggspand-1 (form &optional env)
  "Like MACROEXPAND-1, but performs macroeggspansion instead."
  (flet ((hook (expander form env)
           `(progn
              "MACROEXPANDINGTHINGSCANBEVERYUSEFULYOUKNOW"
              ,(funcall expander form env))))
    (let ((*macroexpand-hook* #'hook))
      (macroexpand-1 form env))))

(define-puzzle macroeggspand-1
  "MACROEXPANDINGTHINGSCANBEVERYUSEFULYOUKNOW")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOT-WHAT-IT-SEEMS

(defclass turbo-special-class (standard-class) ()
  (:documentation "BUTALSOPLEASELOOKINTOCLASSDEFINITIONSYOUKNOW"))

(defmethod sb-mop:validate-superclass ((c turbo-special-class)
                                       (s standard-class))
  t)

(defclass not-what-it-seems ()
  ((value :initform "PLEASEDONOTBREAKPRINTREADCONSISTENCY"))
  (:documentation "Keep looking. Your egg is one meta-level higher.")
  (:metaclass turbo-special-class))

(defmethod print-object ((object not-what-it-seems) stream)
  (princ 'not-what-it-seems stream))

(defun not-what-it-seems ()
  (make-instance 'not-what-it-seems))

(define-puzzle not-what-it-seems
  "PLEASEDONOTBREAKPRINTREADCONSISTENCY"
  "BUTALSOPLEASELOOKINTOCLASSDEFINITIONSYOUKNOW")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IMPATIENCE-IS-A-VIRTUE

(defun impatience-is-a-virtue ()
  "You don't have all day."
  (flet ((foo ()
           (format t ";; Loading... Please wait.~%")
           (sleep 1)
           (loop for i from 5 downto 1
                 do (format t ";; ~D...~%" i)
                    (sleep 1))
           (format t ";; Done. Thank you for your patience.~%"))
         (bar ()
           (format t ";;~%;; \"~A~A\"~%"
                   (funcall (str "WHYDIDYOUINTERRUPTME"))
                   (funcall (str "IWASINTHEMIDDLEOFSOMETHING")))))
    (let ((flag t))
      (unwind-protect (multiple-value-prog1 (foo) (setf flag nil))
        (when flag (bar))))))

(define-puzzle impatience-is-a-virtue
  "WHYDIDYOUINTERRUPTMEIWASINTHEMIDDLEOFSOMETHING")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BOHEMIAN-RHAPSODY

(defun bohemian-rhapsody (&rest args
                          &key high low go come wind-direction
                            (mamaaaaaaaa :ooo-oo-oo-oooooohhhh))
  "Is this the real life? Is this just fantasy?"
  (declare (type (member :easy :medium :hard) come go))
  (declare (type (real (0)) high low))
  (declare (type (member :north :south :east :west
                         :northeast :northwest :southeast :southwest)
                 wind-direction))
  (declare (ignore wind-direction mamaaaaaaaa))
  (unless (<= 10 (length args))
    (error (funcall (str "Too few lyrics provided."))))
  (unless (and (eq (nth 0 args) :come)
               (eq (nth 2 args) :go)
               (eq (nth 4 args) :high)
               (eq (nth 6 args) :low)
               (eq (nth 8 args) :wind-direction))
    (error (funcall (str "Invalid order of lyrics."))))
  (unless (and (eq :easy come)
               (eq :easy go)
               (< 0 high 1)
               (< 0 low 1))
    (error (funcall (str "Invalid lyric values."))))
  (funcall (str "LETFREDDIEMERCURYLIVEFOREVERINOURHEARTS")))

(define-puzzle bohemian-rhapsody
  "LETFREDDIEMERCURYLIVEFOREVERINOURHEARTS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOCKED-AND-UNOPENABLE

(defgeneric locked-and-unopenable (&key)
  (:documentation "How do you insert a &KEY where there is no &KEY&WHOLE?")
  (:method (&rest args &key)
    (when args
      (destructuring-bind (&key (key nil keyp) &allow-other-keys) args
        (unless keyp (error (funcall (str "Must provide a key."))))
        (let ((orig-key key))
          (check-type key (eql :very-golden-key))
          (if (eq key orig-key)
              (funcall (str "YOUAREGETTINGGOODATLOCKPICKING"))
              (funcall (str "DOINGASWITCHEROOATTHELASTMOMENT"))))))))

(define-puzzle locked-and-unopenable
  "YOUAREGETTINGGOODATLOCKPICKING"
  "DOINGASWITCHEROOATTHELASTMOMENT")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CALL-WITH-TEMPORARY-PACKEGGE

(defun call-with-temporary-packegge (function)
  ";; TODO: make use of UNWIND-PROTECT
;; DONE by mhr on 24-01-2025 (cleanup now works properly)"
  (let ((package (make-package (funcall (str "PACKAGENAMESCANBEEGGSTOOYOUKNOW"))
                               :use '())))
    (intern (funcall (str "ANDSYMBOLNAMESCANMAKEVERYGOODEGGSASWELL")) package)
    (unwind-protect (let ((*package* package))
                      (funcall function))
      (delete-package package))))

(define-puzzle call-with-temporary-packegge
  "PACKAGENAMESCANBEEGGSTOOYOUKNOW"
  "ANDSYMBOLNAMESCANMAKEVERYGOODEGGSASWELL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EYES-WIDE-SHUT

(defvar *eyes-wide-shut* t)

(defun eyes-wide-shut (function)
  "Pass in a variant of FUNCALL that locally unbinds *EYES-WIDE-SHUT*."
  (unless (boundp '*eyes-wide-shut*)
    (error (funcall (str "*EYES-WIDE-SHUT* must be bound ~
                          outside EYES-WIDE-SHUT."))))
  (let ((flag t))
    (flet ((fn ()
             (when (null flag)
               (error (funcall (str "This function must not be leaked ~
                                     outside the dynamic scope of ~
                                     EYES-WIDE-SHUT."))))
             (when (boundp '*eyes-wide-shut*)
               (error (funcall (str "*EYES-WIDE-SHUT* must be unbound ~
                                     inside EYES-WIDE-SHUT."))))
             (prog1 (funcall (str "LOCALLYUNBOUNDVARIABLESAREAREALTHING"))
               (when (boundp '*eyes-wide-shut*)
                 (error (funcall (str "*EYES-WIDE-SHUT* must be unbound ~
                                     inside EYES-WIDE-SHUT.")))))))
      (unwind-protect (funcall function #'fn)
        (setf flag nil)
        (unless (boundp '*eyes-wide-shut*)
          (error (funcall (str "*EYES-WIDE-SHUT* must be bound ~
                          outside EYES-WIDE-SHUT."))))))))

(define-puzzle eyes-wide-shut
  "LOCALLYUNBOUNDVARIABLESAREAREALTHING")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; POOF-AND-ITS-GONE

(defparameter *poof-storage*
  "SYMBOLSCANPRETENDTOBEVARIABLESBUTBEMACROSINSTEAD")

(defmacro poof-and-its-gone ()
  `(progn
     (setf *poof-storage* ";; This egg has been removed.")
     (format t ";; Unauthorized access to this egg has been intercepted.
;; This value has now been removed from Lisp memory for everyone's safety.
;; Please authorize yourself and restart this Lisp image.")))

(define-symbol-macro *poof-and-its-gone*
    (progn
      (cerror "Continue anyway." "WARNING: Unauthorized access detected.
The egg is protected from unauthorized access and may be destroyed in case of
a major security incident. (Yes, like the one happening right now.)")
      (poof-and-its-gone)))

(setf (documentation '*poof-and-its-gone* 'variable)
      "CAUTION! Highly volatile egg stored inside. DO NOT EVALUATE.")

(define-puzzle *poof-and-its-gone*
  "SYMBOLSCANPRETENDTOBEVARIABLESBUTBEMACROSINSTEAD")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EGG-ROLLER

(defparameter *egg-roller-matrix*
  '("THEWORSTGAME"
    "OHMYHONEYYES"
    "MYEGGSARESOY"
    "SALSANOTALSA"
    "THESERHYMERS"
    "ALLTEASGREEN"
    "THEANGRYBIRD"
    "AFANGBYEARLS"
    "FARENTERNEAR"
    "UNLEMONYTONE"
    "FAIRNESSANTI"
    "ENGINEERROAR")
  "The egg dough upon which you can roll the egg roller.")

(let ((x -1))
  (defun reset-egg-roller ()
    "Resets the egg roller to the starting position."
    (setf x -1)
    t)
  (defun egg-roller (sequence)
    "Imagine an egg rolling pin. You move it forwards, then backwards,
to roll your egg dough."
    (elt sequence (setf x (mod (1+ x) 12)))))

(define-puzzle egg-roller
  "THESEARENOTREALEGGSYASEE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Epilogue
;;;
;;; Unintern some symbols to prevent them from being easily accessible
;;; via the package system.
;;; Prevents people inspecting the image from seeing too much.

(unintern '*poof-storage*)
(unintern 'poof-and-its-gone)
(unintern '*puzzles*)
(unintern 'define-puzzle)
