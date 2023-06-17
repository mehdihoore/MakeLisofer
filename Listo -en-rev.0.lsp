(defun EZA ()
  ;; Prompt the user to enter a scale number for division on the final AddSum
  (setq scale (getreal "put scale number for division on final AddSum:"))

  ;; Define the error handling function
  (defun myerror (msg)
    (setvar 'lunits *lunits)
    (setq *error* myerror)
    )

  ;; Display a message asking the user to press Enter after finishing
  (princ "Press Enter after finish")

  ;; Set up error handling
  (setq *error* myerror)
  (setq *lunits (getvar 'lunits))
  (setvar 'lunits 4) ; Accept any input format, including scientific and architectural formats

  ;; Check if addsum is already a number
  (if (numberp addsum)
      (progn
        ;; Prompt the user to enter a new value or start with the existing AddSum
        (princ (strcat"\nEnter a number or distance, or [Enter] to start with " (rtos AddSum) ": "))
        (princ (strcat "\n= " (rtos 0 *lunits) " + "))
        (initget 32) ; Allow null, 0, and negatives
        (setq value (getdist))
        (if (/= value nil) (setq addSum value))
        )
      (progn
        ;; Prompt the user to enter a number or specify a distance
        (princ  "\nEnter a number or specify a distance: ")
        (setq addSum 0)
        )
    )

  (setq value 0)
  ;; Continue looping until the user enters a nil value (presses Enter without input)
  (while (/= value nil)
    (princ (strcat "\n= " (rtos AddSum *lunits) " + "))
    (initget 32)
    (setq value (getdist))
    (if value
      (progn
        ;; Add the value to AddSum
        (setq AddSum (+ AddSum value))
        )
      )
    )

  ;; Restore the original linear units setting
  (setvar 'lunits *lunits)

  ;; Print the final value of AddSum with the specified format
  (princ (strcat "\nAddSum = " (rtos AddSum *lunits) "  \[" (rtos AddSum 2 8) "]"))
  (if (or(equal scale "")(equal scale nil))(setq scale 1))
  ;; Divide AddSum by the scale number entered by the user
  (setq AddSum (/ addSum scale))

  (princ) ; Print a newline character
  )

;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
(defun calculate () 
    (setq nvalid 0)

   (while (= nvalid 0) 
     (setq cal "")
     (setq cal (getstring cal))
     
    (if 
      (or (setq s (vl-string-position (ascii "*") cal))
      (setq s (vl-string-position (ascii "+") cal))
      (setq s (vl-string-position (ascii "-") cal))
      (setq s (vl-string-position (ascii "/") cal)))
   
    (progn
     (setq num1 (atof (substr cal 1 (+ s 1) )))
    (setq num2 (atof (substr cal  (+ s 2) (strlen cal))))
      (setq operator (substr cal (+ s 1) 1 ))
      (if(equal operator "+") (setq natijeh(+ num1 num2)))
    (if (equal operator "-") (setq natijeh(- num1 num2)))
    (if (equal operator "*") (setq natijeh(* num1 num2)))
    (if (equal operator "/") (setq natijeh(/ num1 num2)))
    (alert (strcat "natijeh is: " (rtos natijeh 2 2)))
      (setq nvalid 1) 
    ) 
      (progn
      (prompt "Operator is not valid")
        (setq nvalid 0) 
      )))
  )
;;=======================================================================================================================
;; This function calculates the total length of selected entities or based on user input.
;;The code calculates the total length of selected entities or based on user input. 
;;It first sets the system variable "cmdecho" to 0 to suppress command echoing.
;;The code defines three helper functions: getArc, getLine, and getPoly. 
;;getArc uses the lengthen command to get the length of an arc entity.
;; getLine calculates the length of a line entity based on its start and end points. getPoly uses 
;;the area command to get the perimeter of a polygonal entity.
;;The code prompts the user to choose between inputting distances directly or selecting shapes. 
;;If the user selects shapes, the code checks for a valid selection set using ssget. 
;;It then initializes variables for the total length and a counter.
;;The code enters a loop to process each selected entity. 
;;It determines the type of each entity using the entget function and calculates the length accordingly 
;;using the helper functions. 
;;The entity type and its length are displayed to the user.
;;If the user chooses to input distances directly, the code prompts for the input using 
;;EZA and assigns the value to AddSum, which represents the total length.
;;After processing the entities or input distance, the code restores 
;;the command echoing by setting "cmdecho" to 1. It then determines the total count of entities based on the counter 
;;value and displays the result using an alert dialog.
;;Finally, the code ends by executing (princ) to return a null value.===========================
;;=======================================================================================================================
(defun sbe ()
  (setvar "cmdecho" 0)

  ;; Function to get the length of an arc entity
  (defun getArc (en)
    (command "lengthen" en "")
    (getvar "perimeter")
  )

  ;; Function to get the length of a line entity
  (defun getLine (en)
    (setq enlist (entget en))
    (distance (cdr (assoc 10 enlist)) (cdr (assoc 11 enlist)))
  )

  ;; Function to get the perimeter of a polygonal entity
  (defun getPoly (en)
    (command "area" "Object" en)
    (getvar "perimeter")
  )

  ;; Prompt the user to choose between using distances or selecting shapes
  (setq entekhab (getstring t "Do you want put dist(d) or select shapes(s) [d/s]"))

  (if (or (equal entekhab "s") (equal entekhab "S"))
    (progn
      ;; User selected to select shapes
      (if (setq eset (ssget))
        (progn
          (setq totalLen 0)
          (setq cntr 0)

          ;; Loop over each selected entity
          (while (< cntr (sslength eset))
            (setq en (ssname eset cntr))
            (setq enlist (entget en))
            (setq enType (cdr (assoc 0 enlist)))

            ;; Determine the entity type and calculate the length accordingly
            (cond
              ((= enType "ARC") (setq len (getArc en)))
              ((= enType "CIRCLE") (setq len (getPoly en)))
              ((= enType "ELLIPSE") (setq len (getPoly en)))
              ((= enType "LINE") (setq len (getLine en)))
              ((= enType "LWPOLYLINE") (setq len (getPoly en)))
              ((= enType "POLYLINE") (setq len (getPoly en)))
              ((= enType "SPLINE") (setq len (getPoly en)))
              (T (setq len 0.0))
            )

            ;; Format and display the entity type and its length
            (while (< (strlen enType) 12) (setq enType (strcat enType " ")))
            (princ enType)
            (princ "\n Found ")
            (princ " with a length of: ")
            (princ (rtos len))

            ;; Update the total length and increment the counter
            (setq totalLen (+ totalLen len))
            (setq cntr (+ cntr 1))
          )
        )
      )
    )
    (progn
      ;; User selected to input distances
      (princ "put dist:")
      (EZA)
      (setq totalLen AddSum)
    )
  )

  ;; Restore command echo
  (setvar "cmdecho" 1)

  ;; Get the total count of entities and display the result
  (setq cntr (if cntr cntr 0))
  (alert (strcat "\n Found " (itoa cntr) " entitie(s) with a Total Length of " (rtos totalLen)))
  (princ)
)

;;=======================================================================================================================
;; This function splits a string into lines based on specific characters.
;;The code takes a string str as input and splits it into lines based 
;;on specific delimiter characters ("B", "C", "S", "F", "W"). 
;;It initializes an empty list lines to store the resulting lines and an 
;;empty string line to keep track of the current line being built.
;;=======================================================================================================================

(defun split-string-to-lines (str)
  (vl-load-com)

  ;; Initialize the variable to store the lines
  (setq lines '())

  ;; Initialize the variable to store the current line
  (setq line "")

  ;; Initialize the variable to keep track of the string index
  (setq i 1)

  ;; Get the length of the input string
  (setq leng (strlen str))

  ;; Iterate over each character in the string
  (while (< i leng)
    (setq ch (substr str i 1))

    ;; Check if the character is a delimiter (B, C, S, F, W)
    (if (or (equal ch "B") (equal ch "C") (equal ch "S") (equal ch "F") (equal ch "W"))
        (progn
          ;; Trim any trailing newline character from the line
          (setq line (vl-string-right-trim "\n" line))

          ;; Add the line to the list of lines
          (setq lines (cons line lines))

          ;; Reset the line variable for the next line
          (setq line "")
        )
    )

    ;; Append the character to the current line
    (setq line (strcat line ch))

    ;; Move to the next character
    (setq i (+ i 1))
  )

  ;; Trim any trailing newline character from the last line
  (setq line (vl-string-right-trim "\n" line))

  ;; Add the last line to the list of lines
  (setq lines (cons line lines))

  ;; Reverse the order of lines to maintain the original order
  (reverse lines)
)

;;=======================================================================================================================
;;The find-max-number defun includes the logic to check for missing numbers and assigns the appropriate value to max-number. 
;If there is a missing number, it updates max-number to be the missing number. ;
;If there are no missing numbers, it increments max-number by 1.
;;=======================================================================================================================
(defun find-max-number ()
  (setq sel (ssget "_X" '((0 . "TEXT")(8 . "steels"))))
  (setq contents "")
  (if sel
      (progn
        (setq nents (sslength sel))
        (setq i 0)
        (while (< i nents)
          (setq ent (ssname sel i))
          (setq contents (strcat contents (vla-get-TextString (vlax-ename->vla-object ent)) "\n"))
          (setq contents (substr contents 1 (1- (strlen contents))))
          (setq i (1+ i))))
    (prompt "\nNo text found in layer 'steels'."))
  (setq lines (split-string-to-lines contents))

  (setq pos '())
  (foreach line lines
    (setq s (vl-string-position (ascii "/") line))
    (if s
        (progn
          (setq second (substr line (+ s 2) (strlen line)))
          (setq s1 (vl-string-position (ascii "/") second))
          (setq poses (atof (substr line (+ s 2) s1)))
          (setq pos (cons poses pos))
        )
    )
  )

  (setq min-number (apply 'min pos))
  (setq max-number (apply 'max pos))

  (setq missing-numbers '())
  (setq i min-number)
  (while (<= i max-number)
    (if (not (member i pos))
        (setq missing-numbers (cons i missing-numbers))
    )
    (setq i (1+ i))
  )
  (setq missing-num (apply 'min missing-numbers))
  (if (and (setq missing-number  missing-num)(> missing-number 0))
  
      (setq max-number missing-number)
      (setq max-number (+ max-number 1))
  )

 max-number
)

;;====================================================================================================================
;; This function calculates the sums of quantities for each pose in a list of poseRebars.
;;====================================================================================================================
(defun calculate-pose-sums (poseRebars)

  ;; Initialize the variable to store the pose sums
  (setq poseSums '())

  ;; Iterate over each item in poseRebars
  (foreach item poseRebars
    (setq pose (car item)
          quantity (cdr item))

    ;; Check if the pose already exists in poseSums
    (setq existingPose (assoc pose poseSums))

    ;; If the pose already exists, update the quantity
    (if existingPose
      (setq existingQuantity (cdr existingPose)
            newQuantity (+ existingQuantity quantity))
      (setq newQuantity quantity))

    ;; Update poseSums by adding the pose and its new quantity
    ;; and removing any duplicates of the pose
    (setq poseSums (cons (cons pose newQuantity) (vl-remove (lambda (x) (equal pose (car x))) poseSums))))
  
  ;; Initialize the variable to store the maximum pose sums
  (setq maxPoseSums '())

  ;; Iterate over each item in poseSums
  (foreach item poseSums
    (setq pose (car item)
          quantity (cdr item))

    ;; Check if the pose already exists in maxPoseSums
    (setq existingPose (assoc pose maxPoseSums))

    ;; If the pose already exists, update the quantity to the maximum value
    (if existingPose
      (setq existingQuantity (cdr existingPose)
            newQuantity (max existingQuantity quantity))
      (setq newQuantity quantity))

    ;; Update maxPoseSums by adding the pose and its new maximum quantity
    ;; and removing any duplicates of the pose
    (setq maxPoseSums (cons (cons pose newQuantity) (vl-remove (lambda (x) (equal pose (car x))) maxPoseSums))))
  
  ;; Initialize the variable to store the unique pose sums
  (setq poslist '())

  ;; Iterate over each item in maxPoseSums
  ;; and add the item to poslist if it doesn't already exist
  (foreach item maxPoseSums
    (if (not (assoc (car item) poslist))
      (setq poslist (cons item poslist))))
  
  ;; Return the final poslist with unique pose sums
  poslist)

;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================

(defun tbl ()
      (setq sel (ssget "_X" '((0 . "TEXT")(8 . "steels"))))
      (setq contents "")
      (if sel
          (progn
            (setq nents (sslength sel))
            (setq i 0)
            (while (< i nents)
              (setq ent (ssname sel i))
              (setq contents (strcat contents (vla-get-TextString (vlax-ename->vla-object ent)) " "))
              (setq contents (substr contents 1 (1- (strlen contents))))
              (setq i (1+ i))))
        (prompt "\nNo text found in layer 'steels'."))
      (setq lines (split-string-to-lines contents))
      (setq lines (cdr lines))
      (setq tab '())
      (setq tbldata '())
      (setq reb '())
      (setq sum '())
      (setq sumw '())
    (foreach line lines
      (if (> (strlen line) 2)
        (progn
      (setq s (vl-string-position (ascii "/") line))
      (if s
        (progn
        (setq first (substr line 1  s ))
        (setq second (substr line (+ s 2) (strlen line)))
        (setq pos (vl-string-position (ascii "/") second))
        (setq pose  (substr line (+ s 2) pos))
          
        (if pos(progn (setq 3th (substr second (+ pos 2) (strlen second)))
        (setq numb (vl-string-position (ascii "/") 3th))
        (setq numbe  (substr 3th 1 numb)))(setq numbe ""))
        (if numb (progn (setq 4th  (substr 3th (+ numb 2) (strlen 3th)))
        (setq diam (vl-string-position (ascii "@") 4th))
        (setq diame  (substr 4th 1 diam))
        
        (if (setq at (vl-string-position (ascii "c") 4th))
          (setq ate  (substr 4th (+ diam 2) (- at (+ diam 1) )))(progn (setq at (vl-string-position (ascii "/") 4th))  
                                                                  (substr 4th (+ diam 2) (- at (+ diam 1) ))))
        )
            (progn (setq diame "")(setq ate "")))
        (if at (progn (setq 5th  (substr 4th (+ at 4) (strlen 4th)))
        (setq le (vl-string-position (ascii "=") 5th))
        (setq ll (vl-string-position (ascii "/") 5th))
        (setq leng (substr 5th (+ le 2) (- ll (+ le 1) ))))(setq leng ""))
        (if ll (progn (setq 6th  (substr 5th (+ ll 2) (strlen 5th)))
        (setq to (vl-string-position (ascii "m") 6th))
        (setq tota (vl-string-position (ascii "/") 6th))
        (setq totalle (substr 6th 1  to   )))(setq totalle ""))
        (if tota (progn (setq 7th(substr 6th (+ tota 2) (strlen 6th)))
        (setq w (vl-string-position (ascii "k") 7th))
        (setq we (vl-string-position (ascii "/") 7th))
        (setq weig (substr 7th 1  w   )))(setq weig ""))
        (if we (progn (setq 8th(substr 7th (+ we 2) (strlen 7th))))(setq 8th ""))
        (setq sumw (cons (atof weig) sumw))
        
        (setq diame(vl-string-trim " " diame))
        (setq tab (list first pose numbe diame ate leng totalle weig 8th))
        (setq tbldata (append (list tab) tbldata))
        (setq sum (cons diame (atof totalle) ))
        (setq reb (cons sum reb))
        )
      )
    ))) 
    
  
  (setq sumw (apply '+ sumw))
  (setq sumwei   (strcat "Total(kg):" (rtos sumw 2 2)) )
  (setq lis (calculate-pose-sums reb))
  (setq weight-list  '(("Ø8"  "0.617") ("Ø10"  "0.617") ("Ø12"  "0.888") ("Ø14"  "1.21") ("Ø16"  "1.58") ("Ø18" "2.00") ("Ø20" "2.47") ("Ø22" "2.98") ("Ø25" "3.85") ("Ø28"  "4.83") ("Ø30"  "5.55") ("Ø32"  "6.31")))
  (setq fibdata '())                            
  (foreach a lis
        
        (setq keys (car a))
       
        (setq value (rtos(cdr a)2 2))
       
     (setq unit-weight (cdr(assoc keys weight-list)))
        (if (null unit-weight)
        (progn
          (setq weight (getreal (strcat "Enter weight for " keys ": ")))
          (setq weight-list (cons (list keys (rtos weight 2 4)) weight-list))
          (setq unit-weight (cdr (assoc keys weight-list)))
        )
      )
        
        (setq weight (* (atof value) (atof (car unit-weight))))
        (setq ad (list keys  value (rtos (atof(car unit-weight)) 2 2 ) (rtos weight 2 2 ) ))
        (setq fibdata (cons ad fibdata))
    
  )
   
     (setq fname (getstring "first column:<Object> "))
        (setq sname (getstring "second column:<pose>"))
        (setq thirth (getstring "3th column:<QTY> "))
        (setq fourth (getstring "4th column:<Diameter> "))
        (setq fifth (getstring "5th column:<@> "))
        (setq sixth (getstring "6th column:<Length> "))
        (setq seventh (getstring "7th column:<Total(m)> "))
        (setq eigthth (getstring "8th column:<Total(kg)> "))
        (setq nineth (getstring "9th column:<Position> "))
        (setq table-name (getstring "table name: "))
        (if (or (equal fname "")(equal fname nil))(setq fname "Object" ))
        (if (or(equal sname "")(equal sname nil))(setq sname "pose" ))
        (if (or(equal thirth "")(equal thirth nil))(setq thirth "QTY" ))
        (if (or(equal fourth "")(equal fourth nil))(setq fourth "Diameter" ))
        (if (or(equal fifth "")(equal fifth nil))(setq fifth "@" ))
        (if (or(equal sixth "")(equal sixth nil))(setq sixth "Length" ))
        (if (or(equal seventh "")(equal seventh nil))(setq seventh "Total(m)" ))
        (if (or(equal eigthth "")(equal eigthth nil))(setq eigthth "Total(kg)" ))
        (if (or(equal nineth "")(equal nineth nil))(setq nineth "Position" ))
        (if (or(equal table-name "")(equal table-name nil))(setq table-name "Listofer" ))
        (setq column-names (list fname sname thirth fourth fifth sixth seventh eigthth nineth))
        (setq Totalcolumn-names (list fourth seventh "Unit-Weigth(kg/m)" eigthth))    
        (setq tbldata (cons column-names tbldata))
  
         (setq summi  (list " " " " " "  sumwei))
        (setq fibdata (append  fibdata (list summi)))
        (setq fibdata (cons Totalcolumn-names fibdata ))

        
        (defun lst2table ( layer-name lst / pt as cols rh cw ttl data rows sty tbl r k )
        (vl-load-com)
        (setq rh
          (vla-gettextheight
            (setq sty
              (vla-item
                (vla-item (vla-get-dictionaries (vla-get-activedocument (vlax-get-acad-object))) "acad_tablestyle")
                (getvar 'ctablestyle)
              )
            )
            acdatarow
          )
        )
        (setq pt (vlax-3d-point(getpoint "\nInsertion Point: "))
              as (vla-get-block (vla-get-activelayout (vla-get-activedocument (vlax-get-acad-object))))
              cols (if (listp (caadr lst)) (length (caadr lst)) (length (car lst)))
              ttl (if (not (listp (car lst))) (car lst))
              data (if (not (listp (car lst))) (cadr lst) lst)
              data (mapcar '(lambda ( x ) (mapcar '(lambda ( y ) (if (null y) "" y)) x)) data)
              rows (if (not (listp (car lst))) (1+ (length data)) (length data))
        );setq
        (if ttl
          (vla-enablemergeall sty "Title" :vlax-true)
          (vla-enablemergeall sty "Title" :vlax-false)
        )
        (setq cw (apply 'max (mapcar '(lambda ( x ) (apply 'max (mapcar 'strlen x))) (apply 'mapcar (cons 'list data)))));setq
        (setq tbl (vla-addtable as pt rows cols (* 2.5 rh) (* 0.8 cw)));active space/ins point/# rows/# cols/row ht/col wid
        (vla-put-layer tbl layer-name)
      (setq layerColor (vla-get-Color (vla-item (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))) layer-name)))
        (vla-put-Color tbl layerColor)
        (if (vlax-property-available-p tbl 'regeneratetablesuppressed t)
          (vla-put-regeneratetablesuppressed tbl :vlax-true)
        )
        (vla-put-stylename tbl (getvar 'ctablestyle))
        (if ttl
          (progn
            (vla-settext tbl 0 0 ttl)
            (setq r 1)
          )
          (setq r 0)
        )
        (foreach i data
          (setq k -1)
          (foreach ii i
            (vla-settext tbl r (setq k (1+ k)) ii);table/row/col/text
            (cond
              ( (and ttl (> r 1))
                (vla-setcellalignment tbl r k acmiddleleft)
              )
              ( (and (not ttl) (= r 0))
                nil
              )
              ( t
                (vla-setcellalignment tbl r k acmiddleleft)
              )
            )
          );foreach
          (setq r (1+ r))
        );foreach
        (setq cw (mapcar '(lambda ( x ) (apply 'max (mapcar 'strlen x))) (apply 'mapcar (cons 'list data))))
        (setq k -1)
        (foreach c cw
          (vla-setcolumnwidth tbl (setq k (1+ k)) (* c rh 1.25))
        );foreach
        (if (vlax-property-available-p tbl 'regeneratetablesuppressed t)
          (vla-put-regeneratetablesuppressed tbl :vlax-false)
        )
        (vla-update tbl)
        (princ)
      );defun
        (lst2table "steels" (append (list table-name) (list tbldata)) )
       
        (lst2table "steels" (append (list table-name) (list fibdata) ) )
)
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
 (defun calculate-overlap ()
               

                  (setq botF (/ (cdr (assoc dia '((8 . 40) (10 . 40) (12 . 50) (14 . 60) (16 . 65) (18 . 75)
                                                  (20 . 80) (22 . 95) (25 . 120),(28 . 150) (30 . 175) (32 . 200))))))
                  (setq topF (/ (cdr (assoc dia '((8 . 50) (10 . 50) (12 . 60) (14 . 70) (16 . 80) (18 . 90)
                                                  (20 . 100) (22 . 120) (25 . 160) (28 . 200) (30 . 225) (32 . 260))))))
                  (setq column (/ (cdr (assoc dia '((8 . 50) (10 . 50) (12 . 60) (14 . 70) (16 . 80) (18 . 90) 
                                                    (20 . 100) (22 . 110) (25 . 125) (28 . 140) (30 . 150) (32 . 160))))))
                  (setq topB (/ (cdr (assoc dia '((8 . 55) (10 . 65) (12 . 80) (14 . 95) (16 . 105) (18 . 120) 
                                                  (20 . 130) (22 . 180) (25 . 205) (28 . 230),(30 . 245) (32 . 260))))))
                  (setq otherB (/ (cdr (assoc dia '((8 . 40) (10 . 50) (12 . 60) (14 . 70) (16 . 80) (18 . 90) 
                                                    (20 . 100) (22 . 140) (25 . 160) (28 . 175) (30 . 190) (32 . 200))))))
                        
                            
                  
                            (if (or (equal nameOfObject "b") (equal nameOfObject "B") )
                          (progn 
                            (setq vali '("T" "B"))
                        (setq beam (strcase (getstring  t "\nIs it top or bottom of the beam? [T/B]")))
                              (while (not (member   beam   vali))
                                  (setq beam (strcase (getstring  t "\nIs it top or bottom of the beam? [T/B]")))
                              )
                      (if (or (equal beam "T" ) (equal beam "t" ))
                        (progn
                 
                          (setq overlap-length topB)
                        )
                        (progn
                     
                          (setq overlap-length otherB)
                        )
                      )
                            ))
                            (if (or (equal nameOfObject "f") (equal nameOfObject "F") )
                       (progn     
                        (setq foundation (strcase (getstring t  "\nIs it top or bottom of the foundation [T/B]")))  
                              (while (not (member   foundation   vali))
                                  (setq foundation (strcase (getstring t  "\nIs it top or bottom of the foundation [T/B]")))
                              ) 
                      (if (or (equal foundation "T" ) (equal foundation "t" ))
                        (progn
                   
                          (setq overlap-length topF)
                        )
                        (progn
                      
                        (setq overlap-length botF)
                        
                        )
                      )
                            ))
                            (if (or (equal nameOfObject "s") (equal nameOfObject "S") )
                              
                        (progn (setq slab (strcase (getstring  t "\nIs it top or bottom of the Slab [T/B]")))  
                          (while (not (member   slab   vali))
                                  (setq slab (strcase (getstring  t "\nIs it top or bottom of the Slab [T/B]")))
                              ) 
                      (if (or (equal slab "T" ) (equal slab "t" ))
                        (progn
                  
                          (setq overlap-length topF)
                        )
                        (progn
                    
                          (setq overlap-length botF)
                        )
                      )
                            ))
                            
                            (if (or (equal nameOfObject "w") (equal nameOfObject "W") )
                        (progn (setq wvali '("O" "I"))      
                        (setq wall (strcase(getstring t "\nIs it outside or inside of the wall? [O/I]")))
                              (while (not (member   wall   wvali))
                                  (setq wall (strcase(getstring t "\nIs it outside or inside of the wall? [O/I]")))
                              ) 
                      (if (or (equal wall "O" ) (equal wall "o" ))
                        (progn
                   
                          
                          (setq overlap-length topB)
                        )
                        (progn
                    
                          (setq overlap-length otherB)
                        )
                      ))
                            )
                            (if (or (equal nameOfObject "COLUMN")(equal nameOfObject "C")(equal nameOfObject "column"))
                          
                              (setq overlap-length column)
                            )
                      
                  overlap-length )

;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
(defun create-stirrup-block (apt tool arz ht blockname)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq mspace (vla-get-modelspace doc))
  (setq tt 0)
  (setq aa 0)
  (setq text1po 0)
  (setq text2po 0)
  (setq 1po 0 )
  (setq 2po 0 )

  ; Set tt and aa values based on tool and arz
  (if (equal tool arz)
      (progn
        (setq tt 0.6)
        (setq aa 0.6))
      (progn
        (setq tt 0.6)
        (setq aa 0.50)))

  (setq prevX (car apt))
  (setq prevY (cadr apt))
  (setq prevZ (caddr apt))
  (setq offset -1)
  (setq newY (+ prevY offset))
  (setq minVal (min (/ aa 10) (/ tt 10)))
  (setq nx (+ prevX minVal))
  (setq newstartX (list nx prevY prevZ))
  (setq ny (- prevY minVal))
  (setq newstartY (list prevX ny prevZ))
  (setq newApt (list prevX newY prevZ))
  
  (command-s "._rectangle" "Fillet" minVal apt "Dimensions" tt aa newApt)
  (setq rectangle (entlast))
  
  (setq newEndX (list (+ nx (* (- aa minVal) 0.40)) (- prevY (* (- tt minVal) 0.40)) prevZ))
  (setq newEndY (list (+ prevX (* aa 0.5580)) (- ny (* (- tt minVal ) (/ 3906244.3855 (- ny 0.0)))) prevZ))
  
  (command-s "._line" newstartY  newEndX "<135" "" "" "")
  (setq linek2 (entlast))
  
  (command "._offset" "" linek2 newstartX  "" "")
  (setq linek1 (entlast))

  (setq LayerName "steels")
  (setq nny (+ prevY (* 2 ht)))
  
  (setq text1po (list (- prevX (* 0.1 2.6)) (- prevY (/ aa 2)) prevZ))
  (setq text2po (list (+ prevX (/ tt 3))  (+ prevY (/ 0.1 4))   prevZ))
  
  (command-s "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
  (setq text1 (rtos arz 2 2))
  (setq text2 (rtos tool 2 2))
  
  (setq thetext1 (vla-AddText mspace text1 (vlax-3d-point text1po) 0.1))
  (vla-put-layer thetext1 LayerName)
  (setq texts1 (entlast))
  
  (setq thetext2 (vla-AddText mspace text2 (vlax-3d-point text2po) 0.1))
  (vla-put-layer thetext2 LayerName)
  (setq texts2 (entlast))

  ; Assign the rectangle and lines to a specific layer
  (vla-put-layer (vlax-ename->vla-object rectangle) LayerName)
  (vla-put-layer (vlax-ename->vla-object linek1) LayerName)
  (vla-put-layer (vlax-ename->vla-object linek2) LayerName)
  (vla-put-layer (vlax-ename->vla-object texts1) LayerName)
  (vla-put-layer (vlax-ename->vla-object texts2) LayerName)

  (setq startss (list (+ prevX (* 2 tt)) (+ prevY (* 4 0.1)) prevZ))
  (setq endss (list (- prevX (* 0.1 2.5)) (- prevY (* 2 aa)) prevZ))
  (setq 1po (list (- prevX (* 0.1 3)) (- prevY  (* aa 2) ) prevZ))
  (setq 2po (list (+ prevX tt )  (+ prevY 1)   prevZ))
  (setq blockpoints (ssget "_W" 1po 2po))

  (prompt "\nSelect Objects for Make Block")
  (setq selection (ssget))
  
  ; Check if at least 3 objects are selected
  (while (or (equal selection nil) (< (sslength selection) 3))
    (princ "Please select at least 3 objects.")
    (setq selection (ssget))
  )

  (command-s "._-block" blockname text1po selection "")
  (setq blockref (entlast))
  (vla-put-layer (vlax-ename->vla-object blockref) LayerName)

  ; Insert the block at the specified scale
  (setq scale (* ht 10)) ; Set the desired scale factor here
  (command "._insert" blockname text1po scale scale "")
  (setq inref (entlast))
  (vla-put-layer (vlax-ename->vla-object inref) LayerName)
)

;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
(defun create-135*Sanjaghi-block (apt tool kham ht blockname)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq mspace (vla-get-modelspace doc))
  (setq tt 0)
  (setq aa 0)
  (setq text1po 0)
  (setq text2po 0)
  (setq 1po 0 )
  (setq 2po 0 )

  (setq prevX (car apt))
  (setq prevY (cadr apt))
  (setq prevZ (caddr apt))
  (setq offset (* ht 2))
  (setq newY (+ prevY offset))
  (setq minVal (min (/ aa 10) (/ tt 10)))
  (setq nx (+ prevX minVal))
  (setq newstartX (list nx prevY prevZ))
  (setq ny (- prevY minVal))
  (setq newstartY (list prevX ny prevZ))
  (setq newApt (list prevX newY prevZ))
  (setq endpoint (list (+ 0.50 prevX) newY prevZ))
  (setq endpoint1 (list (+ prevX 0.16) (- newY 0.1) prevZ))
  (setq endpoint2 (list (+ prevX 0.34 ) (- newY 0.1) prevZ))
  (command-s "._line" newApt  endpoint  "" "" "") ; Create line
  (setq line1 (entlast))
  (command-s "._line" newApt "<135"  endpoint1  "" "" "") ; Create line
  (setq line2 (entlast))
  (command-s "._line" endpoint "<45"  endpoint2  "" "" "") ; Create line
  (setq line3 (entlast))
  (command "._fillet" line1 "Radius" 0.02 "" line2) ; Fillet the lines
  (command "._fillet" line1 "Radius" 0.02 "" line3) ; Fillet the lines
  (defun DtR (d) (* pi (/ d 180.0)))
  (setq endpoint4 (list (+ prevX 0.16) (- newY 0.12) prevZ))
  (setq endpoint5 (list (+ prevX 0.30 ) (- newY 0.12) prevZ))
  (setq midpoint (polar endpoint4 (DTR 135) (/ (distance newApt endpoint1) 2.0) ))
  (setq midpoint1 (polar endpoint5 (DTR 45) (/ (distance newApt endpoint1) 2.0) ))

  (setq text1po  (list (+ 0.20 prevX) (+ newY (* (/ ht 15) 2)) prevZ) )

  (command-s "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
  (setq text1 (rtos tool 2 2))
  (setq text2 (rtos kham 2 2))
  (setq text3 (rtos kham 2 2))
  (setq thetext1 (vla-AddText mspace text1 (vlax-3d-point text1po) 0.05))
  (vla-put-layer thetext1 LayerName)
  (setq texts1 (entlast))
  (setq thetext2 (vla-AddText mspace text2 (vlax-3d-point midpoint) 0.03))
  (vla-put-layer thetext2 LayerName)
  (setq texts2 (entlast))
  (setq thetext3 (vla-AddText mspace text3 (vlax-3d-point midpoint1) 0.03))
  (vla-put-layer thetext3 LayerName)
  (setq texts3 (entlast))

  (vla-put-layer (vlax-ename->vla-object line1) LayerName)
  (vla-put-layer (vlax-ename->vla-object line2) LayerName)
  (vla-put-layer (vlax-ename->vla-object line3) LayerName)
  (vla-put-layer (vlax-ename->vla-object texts1) LayerName)
  (vla-put-layer (vlax-ename->vla-object texts2) LayerName)
  (vla-put-layer (vlax-ename->vla-object texts3) LayerName)

  (prompt "\nSelect Objects for Make Block")
  (setq selection (ssget))
  (while (or (equal selection nil) (< (sslength selection) 3))
    (princ "Please select at least 3 objects.")
    (setq selection (ssget))
  )

  (command-s "._-block" blockname text1po selection "")
  (setq blockref (entlast))
  (vla-put-layer (vlax-ename->vla-object blockref) LayerName)

  ; Insert the block at the specified scale
  (setq scale (* ht 10)) ; Set the desired scale factor here
  (command "._insert" blockname text1po scale scale "")
  (setq inref (entlast))
  (vla-put-layer (vlax-ename->vla-object inref) LayerName)
)

;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
(defun create-90*Sanjaghi-block (apt tool kham ht blockname)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq mspace (vla-get-modelspace doc))
  
  ; Calculate the new coordinates for creating lines
  (setq prevX (car apt))
  (setq prevY (cadr apt))
  (setq prevZ (caddr apt))
  (setq offset (* ht 2))
  (setq newY (+ prevY offset))
  (setq newApt (list prevX newY prevZ))
  (setq endpoint (list (+ 0.50 prevX) newY prevZ))
  (setq endpoint1 (list prevX (- newY 0.1) prevZ))
  (setq endpoint2 (list (+ 0.50 prevX) (- newY 0.1) prevZ))
  
  ; Create lines and fillets
  (command-s "._line" newApt endpoint "" "" "")
  (setq line1 (entlast))
  (command-s "._line" newApt "<90" endpoint1 "" "" "")
  (setq line2 (entlast))
  (command-s "._line" endpoint "<90" endpoint2 "" "" "")
  (setq line3 (entlast))
  (command "._fillet" line1 "Radius" 0.02 "" line2) 
  (command "._fillet" line1 "Radius" 0.02 "" line3) 
  
  ; Helper function to convert degrees to radians
  (defun DtR (d) (* pi (/ d 180.0)))
  
  ; Calculate additional points and midpoints
  (setq endpoint4 (list prevX (- newY 0.12) prevZ))
  (setq endpoint5 (list (+ prevX 0.43) (- newY 0.12) prevZ))
  (setq midpoint (polar endpoint4 (DTR 90) (/ (distance newApt endpoint1) 2.0)))
  (setq midpoint1 (polar endpoint5 (DTR 90) (/ (distance newApt endpoint1) 2.0)))
  
  ; Define the position for text1
  (setq text1po (list (+ 0.20 prevX) (+ newY (* (/ ht 15) 2)) prevZ))
  
  ; Set the text style and create text entities
  (command-s "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
  (setq text1 (rtos tool 2 2))
  (setq text2 (rtos kham 2 2))
  (setq text3 (rtos kham 2 2))
  (setq thetext1 (vla-AddText mspace text1 (vlax-3d-point text1po) 0.05))
  (vla-put-layer thetext1 LayerName)
  (setq texts1 (entlast))
  (setq thetext2 (vla-AddText mspace text2 (vlax-3d-point midpoint) 0.03))
  (vla-put-layer thetext2 LayerName)
  (setq texts2 (entlast))
  (setq thetext3 (vla-AddText mspace text3 (vlax-3d-point midpoint1) 0.03))
  (vla-put-layer thetext3 LayerName)
  (setq texts3 (entlast))

  ; Assign layers to entities
  (vla-put-layer (vlax-ename->vla-object line1) LayerName)
  (vla-put-layer (vlax-ename->vla-object line2) LayerName)
  (vla-put-layer (vlax-ename->vla-object line3) LayerName)
  (vla-put-layer (vlax-ename->vla-object texts1) LayerName)
  (vla-put-layer (vlax-ename->vla-object texts2) LayerName)
  (vla-put-layer (vlax-ename->vla-object texts3) LayerName)

  ; Prompt for object selection for block creation
  (prompt "\nSelect Objects for Make Block")
  (setq selection nil)
  
  ; Validate the selection to have at least 3 objects
  (while (or (equal selection nil) (< (sslength selection) 3))
    (setq selection (ssget))
    (princ "Please select at least 3 objects.")
  )

  ; Create block using selected objects
  (command-s "._-block" blockname text1po selection "")
  (setq blockref (entlast))
  (vla-put-layer (vlax-ename->vla-object blockref) LayerName)
  
  ; Prompt for scale input
  (setq scale (* ht 10))
  
  ; Insert the block with the specified scale
  (command "._insert" blockname text1po scale scale "")
  (setq inref (entlast))
  (vla-put-layer (vlax-ename->vla-object inref) LayerName)
)

;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
(defun create-90*135Sanjaghi-block (apt tool kham ht blockname)
  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq mspace (vla-get-modelspace doc))
  
  ; Calculate the new coordinates for creating lines
  (setq prevX (car apt))
  (setq prevY (cadr apt))
  (setq prevZ (caddr apt))
  (setq offset (* ht 2))
  (setq newY (+ prevY offset))
  (setq newApt (list prevX newY prevZ))
  (setq endpoint (list (+ 0.50 prevX) newY prevZ))
  (setq endpoint1 (list prevX (- newY 0.1) prevZ))
  (setq endpoint2 (list (+ prevX 0.34) (- newY 0.1) prevZ))
  
  ; Create lines and fillets
  (command-s "._line" newApt endpoint "" "" "")
  (setq line1 (entlast))
  (command-s "._line" newApt "<90" endpoint1 "" "" "")
  (setq line2 (entlast))
  (command-s "._line" endpoint "<135" endpoint2 "" "" "")
  (setq line3 (entlast))
  (command "._fillet" line1 "Radius" 0.02 "" line2) 
  (command "._fillet" line1 "Radius" 0.02 "" line3) 
  
  ; Helper function to convert degrees to radians
  (defun DtR (d) (* pi (/ d 180.0)))
  
  ; Calculate additional points and midpoints
  (setq endpoint4 (list prevX (- newY 0.12) prevZ))
  (setq endpoint5 (list (+ prevX 0.43) (- newY 0.12) prevZ))
  (setq midpoint (polar endpoint4 (DtR 90) (/ (distance newApt endpoint1) 2.0)))
  (setq midpoint1 (polar endpoint5 (DtR 90) (/ (distance newApt endpoint1) 2.0)))
  
  ; Define the position for text1
  (setq text1po (list (+ 0.20 prevX) (+ newY (* (/ ht 15) 2)) prevZ))
  
  ; Set the text style and create text entities
  (command-s "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
  (setq text1 (rtos tool 2 2))
  (setq text2 (rtos kham 2 2))
  (setq text3 (rtos kham 2 2))
  (setq thetext1 (vla-AddText mspace text1 (vlax-3d-point text1po) 0.05))
  (vla-put-layer thetext1 LayerName)
  (setq texts1 (entlast))
  (setq thetext2 (vla-AddText mspace text2 (vlax-3d-point midpoint) 0.03))
  (vla-put-layer thetext2 LayerName)
  (setq texts2 (entlast))
  (setq thetext3 (vla-AddText mspace text3 (vlax-3d-point midpoint1) 0.03))
  (vla-put-layer thetext3 LayerName)
  (setq texts3 (entlast))

  ; Assign layers to entities
  (vla-put-layer (vlax-ename->vla-object line1) LayerName)
  (vla-put-layer (vlax-ename->vla-object line2) LayerName)
  (vla-put-layer (vlax-ename->vla-object line3) LayerName)
  (vla-put-layer (vlax-ename->vla-object texts1) LayerName)
  (vla-put-layer (vlax-ename->vla-object texts2) LayerName)
  (vla-put-layer (vlax-ename->vla-object texts3) LayerName)

  ; Prompt for object selection for block creation
  (prompt "\nSelect Objects for Make Block")
  (setq selection nil)
  
  ; Validate the selection to have at least 3 objects
  (while (or (equal selection nil) (< (sslength selection) 3))
    (setq selection (ssget))
    (princ "Please select at least 3 objects.")
  )

  ; Create block using selected objects
  (command-s "._-block" blockname text1po selection "")
  (setq blockref (entlast))
  (vla-put-layer (vlax-ename->vla-object blockref) LayerName)
  (setq scale (* ht))
  
  ; Insert the block with the specified scale
  (command "._insert" blockname text1po scale scale "")
  
  (setq inref (entlast))
  (vla-put-layer (vlax-ename->vla-object inref) LayerName)
)

;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================

(defun put-text ()
    (setq objects '("b" "c" "f" "s" "w" "B" "C" "F" "S" "W" ))
    (setq mainlist '("m" "p" "M" "P" ))
    
    (setq position (FIND-MAX-NUMBER))
    (setq mainrebar (strcase(getstring t "\n main (M) Or part (P)[M/P]")))
    (while (not (member   mainrebar   mainlist))
      (setq mainrebar (strcase(getstring t "\n main (M) Or part (P)[M/P]")))
    ) 
  (if (or (equal mainrebar "main") (equal mainrebar "m") (equal mainrebar "M") (equal mainrebar "Main") (equal mainrebar "MAIN"))
    (progn   
      
      (setq em 1)
        (while ( > em 0)
          (setq nameOfObject (strcase(getstring t "\nEnter name of object BEAM/COLUMN/FOUNDATION/SLAB/WALL [B/C/F/S/W]")))
          (while (not (member   nameOfObject   objects))
            (setq nameOfObject (strcase(getstring t " \nEnter name of object BEAM/COLUMN/FOUNDATION/SLAB/WALL [B/C/F/S/W]")))
          )
          (setq diameters '("8" "10" "12" "14" "16" "18" "20" "22" "25" "28" "30" "32"))
              (setq dia (atoi(getstring t "\nSelect diameter [8/10/12/14/16/18/20/22/25/28/30/32]: ")))
              (while (not (member  (rtos dia 2 0)  diameters))
                (setq dia (atoi(getstring t "\nInvalid input. Select diameter [8/10/12/14/16/18/20/22/25/28/30/32]: ")))
              )
          (setq length-choice 0)
          (initget (+ 1 4))
          (setq length-choice (getreal "\nEnter length of rebar or Enter [0] to select"))
          
          (if (= length-choice 0)
            (progn
                  (sbe)
                  (setq length-choice totalLen )
                  (if (> length-choice 12) 
                      (progn
                            (setq overlap-length ( calculate-overlap))
                            (setq overlap-length (/ overlap-length 100.))
                            (princ (strcat "\nOverlap Length: " (rtos overlap-length 2 2) " cm"))
                            (setq number  1)
                            (setq max-length 12.0)  ; Maximum length of a rebar without overlap
                            (setq bend (strcase (getstring t "\nwith bend(b) or without bend(w) [b/w]:")))
                          
                            (if 
                              (or(equal bend "b") (equal bend "B"))
                              (progn
                                (initget (+ 1 4))
                                (setq qty (getreal "put number of rebars or enter [0]"))
                                (if 
                                      (or(= qty 0) (equal qty "0"))
                                  (progn
                                    (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                                    (eza)
                                    
                                    (setq dist AddSum)
                                    (initget (+ 1 2 4))
                                    (setq space (getreal "\nEnter the spacing between rebars in cm: "))
                                    ; Calculate the number of rebars
                                    (setq cmspace (/ space 100 ) )
                                    (setq num (1+ (fix (/ dist cmspace))))
                                  )
                                  (setq num  qty)
                                )
                                
                              
                              
                                
                                  (setq kham (* dia  12))
                                  (setq khamm (/ kham 1000.))
                                  (setq length-choice(- length-choice (- (- max-length khamm)  overlap-length )))
                                (if (or (> length-choice 12)(= length-choice 12))
                                (setq remm(- length-choice (- max-length overlap-length)  ))(setq remm length-choice)
                                )
                                  
                                  (while (> remm max-length )
                                        (setq number (1+ number ))
                                        (setq remm (- length-choice (- (* max-length number) (* overlap-length number))))
                                  )
                                  (setq remm1(+ remm khamm))
                                  (setq totall (+ (* max-length  number)  remm1 ))
                                (alert (strcat "Overlap Length: " (rtos overlap-length 2 2 ) 
                                        " cm. Total count: " (rtos number 2 2 )  ". Number of 12 m rebars:" (rtos number 2 2 ) 
                                        ". Remaining total length: " (rtos remm1 2 2) " kham: "(rtos khamm 2 2) " m"))
                              )
                              (progn
                                (initget (+ 1 4))
                                (setq qty (getreal "put number of rebars or enter [0]"))
                                (if 
                                      (or(= qty 0) (equal qty "0"))
                                  (progn
                                    (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                                    (eza)
                                    (setq dist AddSum)
                                    (initget (+ 1 2 4))
                                    (setq space (getreal "\nEnter the spacing between rebars in cm: "))
                                    ; Calculate the number of rebars
                                    (setq cmspace (/ space 100 ) )
                                    (setq num (1+ (fix (/ dist cmspace))))
                                  )
                                  (setq num  qty)
                                )
                                  (setq remm(- length-choice (- max-length overlap-length)  ))
                                  (while (> remm max-length )
                                        (setq number (1+ number ))
                                        (setq remm (- length-choice (- (* max-length number) (* overlap-length number))))
                                  )
                                
                                  (setq totall (+ (* max-length  number) remm))
                               
                              )
                            )
                                
                            (princ (strcat "Overlap Length: " (rtos overlap-length 2 2 ) 
                                        " cm. Total count: " (rtos number 2 2 )  ". Number of 12 m rebars:" (rtos number 2 2 ) 
                                        ". Remaining total length: " (rtos remm 2 2)))  
                                (if (or(equal space "")(equal space nil)) (setq space 0))
                                (setq position (find-max-number))
                                    (setq tot (* max-length number num))
                                (setq unit-weight (/ (cdr (assoc dia '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
                                          (setq total-weight  (* tot unit-weight))
                                
                                (setq finaltext (strcat  nameOfObject "/"  (rtos position 2 0) "/" (rtos (* num number) 2 0) "/\U+00D8" (rtos dia 2 0) "@" (rtos space 2 0) "cm/"" L=" (rtos max-length 2 2) "/" (rtos tot 2 2) "m/" (rtos total-weight 2 2) " kg/"mainrebar "\n")) 
                                (alert (strcat "this is string: " " "finaltext) )
                          
                          (vl-load-com)
                          (setq mspace (vla-get-modelspace 
                                            (vla-get-activedocument 
                                                  (vlax-get-acad-object))))
                            (initget (+ 1 2 4))
                            (setq apt (getpoint "\nInsertion Point: "))

                            (setq LayerName "steels")
                            
                            (setq ht (getreal "\nHeight : "))
                          (if (or (equal ht nil)(equal ht "")) (setq ht 1)(setq ht ht))
                            (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
                            (setq thetext (vla-AddText mspace finaltext 
                                                          (vlax-3d-point apt) ht))
                          (vla-put-layer thetext LayerName )
                          ;;ppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp
                          (if (or(equal space "")(equal space nil)) (setq space 0))
                            (setq position (find-max-number))
                                (setq toto (* remm num))
                            (setq unit-weight (/ (cdr (assoc dia '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
                                      (setq total-weight1  (* toto unit-weight))
                            
                            (setq finaltextover (strcat  nameOfObject "/"  (rtos  position  2 0) "/" (rtos num 2 0) "/\U+00D8" (rtos dia 2 0) "@" (rtos space 2 0) "cm/"" L=" (rtos remm 2 2) "/" (rtos toto 2 2) "m/" (rtos total-weight1 2 2) " kg/" mainrebar "\n")) 
                            (alert (strcat "this is string: " " "finaltextover) )
                                
                                (vl-load-com)
                                (setq mspace (vla-get-modelspace 
                                                  (vla-get-activedocument 
                                                        (vlax-get-acad-object))))
                            ;;(setq currlayer (vla-get-layer doc))
                          (setq prevX (car apt))
                              (setq prevY (cadr apt))
                              (setq prevZ (caddr apt))

                              ; Set the offset distance for the new text below the previous text
                              (setq offset (* -2 ht))  ; Adjust this value as needed

                              ; Calculate the new Y-coordinate for the insertion point
                              (setq newY (+ prevY offset))

                              ; Create the new insertion point with the updated coordinates
                              (setq newApt (list prevX newY prevZ))

                            (setq LayerName "steels")
                            
                            (setq ht (getreal "\nHeight : "))
                            (if (or (equal ht nil)(equal ht "")) (setq ht 1)(setq ht ht))
                            (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
                            (setq thetextover (vla-AddText mspace finaltextover 
                                                          (vlax-3d-point newApt) ht))
                          (vla-put-layer thetextover LayerName )
                        (setq length-choice 13)
                      )
            
                  )
                  (if (or (< length-choice 12) (= length-choice 12))
                    (progn
                        (setq kham (* dia  12))
                              (setq khamm (/ kham 1000.))
                              (alert  (strcat (rtos length-choice 2 2 ) "+ ((kham) 2 * " (rtos khamm 2 2 ) ")")   )
                              (setq totalLen (+ length-choice (* 2 khamm) ))
                            (initget (+ 1 4))
                            (setq qty (getreal "put number of rebars or enter [0]"))
                              (if 
                                    (or(= qty 0) (equal qty "0"))
                                (progn
                                  (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                                  (eza)
                                  (setq dist AddSum)
                                  (initget (+ 1 2 4))
                                  (setq space (getreal "\nEnter the spacing between rebars in cm: "))
                                  ; Calculate the number of rebars
                                  (setq cmspace (/ space 100 ) )
                                  (setq num (1+ (fix (/ dist cmspace))))
                                  (setq qty num)
                                )
                                (setq num  qty)
                              )
                              
                              (setq tot (* num totalLen))
                              (setq unit-weight (/ (cdr (assoc dia '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
                              (setq total-weight  (* tot unit-weight)) 
                              (if (or(equal space "")(equal space nil)) (setq space 0))
                              (setq position (find-max-number))
                            (setq finaltext (strcat  nameOfObject "/"  (rtos position 2 0) "/" (rtos num 2 0) "/\U+00D8" (rtos dia 2 0) "@" (rtos space 2 0) "cm/"" L=" (rtos totalLen 2 2) "/" (rtos tot 2 2) "m/" (rtos total-weight 2 2) " kg/" mainrebar "\n"))
                              (alert (strcat "this is string: " " "finaltext) )
                                
                                (vl-load-com)
                                (setq mspace (vla-get-modelspace 
                                                  (vla-get-activedocument 
                                                        (vlax-get-acad-object))))
                                  (initget (+ 1 2 4))
                                  (setq apt (getpoint "\nInsertion Point: "))

                                  (setq LayerName "steels")
                                  
                                  (setq ht (getreal "\nHeight : "))
                                  (if (or (equal ht nil)(equal ht "")) (setq ht 1)(setq ht ht))
                                  (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
                                  (setq thetext (vla-AddText mspace finaltext 
                                                                (vlax-3d-point apt) ht))
                                (vla-put-layer thetext LayerName )
                    )
                  ) 
            )

            (progn
              (if (> length-choice 12)
                    (progn
                            (setq overlap-length ( calculate-overlap))
                            (setq overlap-length (/ overlap-length 100.))
                            (princ (strcat "\nOverlap Length: " (rtos overlap-length 2 2) " cm"))
                            (setq number  1)
                            (setq max-length 12.0)  ; Maximum length of a rebar without overlap
                            (setq bend (getstring t "\nwith bend(b) or without bend(w) [b/w]:"))
                            (if 
                              (or(equal bend "b") (equal bend "B"))
                              (progn
                                (initget (+ 1 4))
                                (setq qty (getreal "put number of rebars or enter [0]"))
                                (if 
                                      (or(= qty 0) (equal qty "0"))
                                  (progn
                                    (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                                    (eza)
                                    (setq dist AddSum)
                                    (initget (+ 1 2 4))
                                    (setq space (getreal "\nEnter the spacing between rebars in cm: "))
                                    ; Calculate the number of rebars
                                    (setq cmspace (/ space 100 ) )
                                    (setq num (1+ (fix (/ dist cmspace))))
                                  )
                                  (setq num qty)
                                )
                                (setq num qty)
                              
                                
                                
                                  (setq kham (* dia  12))
                                  (setq khamm (/ kham 1000.))
                                  (setq length-choice(- length-choice (- (- max-length khamm)  overlap-length )))
                                  
                                (if (or (> length-choice 12)(= length-choice 12))
                                (setq remm(- length-choice (- max-length overlap-length)  ))(setq remm length-choice)
                                )
                                  (while (> remm max-length )
                                        (setq number (1+ number ))
                                        (setq remm (- length-choice (- (* max-length number) (* overlap-length number))))
                                  )
                                  (setq remm(+ remm khamm))
                                  (setq length-choice (+ (* max-length  number)  remm max-length))
                                (alert (strcat "Start rebar: " (rtos(- max-length khamm) 2 2 ) "\n+\nbend: " (rtos khamm 2 2 )
                                      "\nEnd rebar: " (rtos (- remm khamm) 2 2) "\n+\nbend: " (rtos khamm 2 2 )))
                              )
                              (progn
                                (initget (+ 1 4))
                                (setq qty (getreal "put number of rebars or enter [0]"))
                                (if 
                                      (or(= qty 0) (equal qty "0"))
                                  (progn
                                    (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                                    (eza)
                                    (setq dist AddSum)
                                    (initget (+ 1 2 4))
                                    (setq space (getreal "\nEnter the spacing between rebars in cm: "))
                                    ; Calculate the number of rebars
                                    (setq cmspace (/ space 100 ) )
                                    (setq num (1+ (fix (/ dist cmspace))))
                                  )
                                  (setq num qty)
                                )
                                (setq num qty)
                                  (setq remm(- length-choice (- max-length overlap-length)  ))
                                  (while (> remm max-length )
                                        (setq number (1+ number ))
                                        (setq remm (- length-choice (- (* max-length number) (* overlap-length number))))
                                  )
                                
                                  (setq length-choice (+ (* max-length  number) remm))
                                (alert (strcat "Start rebar: " (rtos max-length  2 2 ) 
                                      "\nEnd rebar: " (rtos remm 2 2)))
                              )
                            )
                                
                            (princ (strcat "Overlap Length: " (rtos overlap-length 2 2 ) 
                                        " cm. Total count: " (rtos number 2 2 )  ". Number of 12 m rebars:" (rtos number 2 2 ) 
                                        ". Remaining total length: " (rtos remm 2 2)))  
                          ;;yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy
                          (if (or(equal space "")(equal space nil)) (setq space 0))
                            (setq position (find-max-number))
                                (setq tot (* max-length number num))
                            (setq unit-weight (/ (cdr (assoc dia '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
                                      (setq total-weight  (* tot unit-weight))
                            
                            (setq finaltext (strcat  nameOfObject "/"  (rtos position 2 0) "/" (rtos num 2 0) "/\U+00D8 " (rtos dia 2 0) "@" (rtos space 2 0) "cm/"" L=" (rtos max-length 2 2) "/" (rtos tot 2 2) "m/" (rtos total-weight 2 2) " kg/"mainrebar"\n" ))
                            (alert (strcat "this is string: " " "finaltext) )
                                
                                (vl-load-com)
                                (setq mspace (vla-get-modelspace 
                                                  (vla-get-activedocument 
                                                        (vlax-get-acad-object))))
                                  ;;(setq currlayer (vla-get-layer doc))
                                  (initget (+ 1 2 4))
                                  (setq apt (getpoint "\nInsertion Point: "))

                                  (setq LayerName "steels")
                                  
                                  (setq ht (getreal "\nHeight : "))
                              (if (or (equal ht nil)(equal ht "")) (setq ht 1)(setq ht ht))
                                  (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
                                  (setq thetext (vla-AddText mspace finaltext 
                                                                (vlax-3d-point apt) ht))
                                (vla-put-layer thetext LayerName )
                                ;;ppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp
                                (if (or(equal space "")(equal space nil)) (setq space 0))
                            (setq position (find-max-number))
                                (setq toto (* remm num))
                            (setq unit-weight (/ (cdr (assoc dia '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
                                      (setq total-weight  (* toto unit-weight))
                            
                            (setq finaltextover (strcat  nameOfObject "/"  (rtos position 2 0) "/" (rtos num 2 0) "/\U+00D8" (rtos dia 2 0) "@" (rtos space 2 0) "cm/"" L=" (rtos remm 2 2) "/" (rtos toto 2 2) "m/" (rtos total-weight 2 2) " kg/" mainrebar "\n")) 
                            (alert (strcat "this is string: " " "finaltextover) )
                                
                                (vl-load-com)
                                (setq mspace (vla-get-modelspace 
                                                  (vla-get-activedocument 
                                                        (vlax-get-acad-object))))
                                  ;;(setq currlayer (vla-get-layer doc))
                                (setq prevX (car apt))
                                (setq prevY (cadr apt))
                                (setq prevZ (caddr apt))

                                ; Set the offset distance for the new text below the previous text
                                (setq offset (* -2 ht))  ; Adjust this value as needed

                                ; Calculate the new Y-coordinate for the insertion point
                                (setq newY (+ prevY offset))

                                ; Create the new insertion point with the updated coordinates
                                (setq newApt (list prevX newY prevZ))

                                  (setq LayerName "steels")
                                  
                                  (setq ht (getreal "\nHeight : "))
                                (if (or (equal ht nil)(equal ht "")) (setq ht 1)(setq ht ht))
                                  (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
                                  (setq thetextover (vla-AddText mspace finaltextover 
                                                                (vlax-3d-point newApt) ht))
                                (vla-put-layer thetextover LayerName )
                    )
              )
              (if (or (< length-choice 12) (= length-choice 12))
                (progn 
                  (setq totalLen length-choice)
                  (initget (+ 1 4))
                  (setq qty (getstring "put number of rebars or enter [0]"))
                                (if 
                                      (or(= qty 0) (equal qty "0"))
                                  (progn
                                    (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                                    (eza)
                                    (setq dist AddSum)
                                    (initget (+ 1 2 4))
                                    (setq space (getreal "\nEnter the spacing between rebars in cm: "))
                                    ; Calculate the number of rebars
                                    (setq cmspace (/ space 100 ) )
                                    (setq num (1+ (fix (/ dist cmspace))))
                                  )
                                  (progn
                                  (eza)
                                  (setq qty AddSum)
                                  (setq num  qty) 
                                  )
                                  
                                )
                                (setq num qty)
                                (setq tot (* num totalLen))
                                (setq unit-weight (/ (cdr (assoc dia '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
                                (setq total-weight  (* tot unit-weight))
                      
                      (setq kham (* dia  12))
                      (setq khamm (/ kham 1000.))
                      (if (or(equal space "")(equal space nil)) (setq space 0))
                      (setq position (find-max-number))
                      (setq finaltext (strcat  nameOfObject "/"  (rtos position 2 0) "/" (rtos num 2 0) "/\U+00D8" (rtos dia 2 0) "@" (rtos space 2 0) "cm/" " L=" (rtos totalLen 2 2) "/" (rtos tot 2 2) "m/" (rtos total-weight 2 2) " kg/"mainrebar "\n" )) 
                      (alert (strcat "this is string: " " "finaltext) )
                          
                          (vl-load-com)
                          (setq mspace (vla-get-modelspace 
                                            (vla-get-activedocument 
                                                  (vlax-get-acad-object))))
                            ;;(setq currlayer (vla-get-layer doc))
                            (initget (+ 1 2 4))
                            (setq apt (getpoint "\nInsertion Point: "))

                            (setq LayerName "steels")
                            
                            (setq ht (getreal "\nHeight : "))
                          (if (or (equal ht nil)(equal ht "")) (setq ht 1)(setq ht ht))
                            (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
                            (setq thetext (vla-AddText mspace finaltext 
                                                          (vlax-3d-point apt) ht))
                          (vla-put-layer thetext LayerName )
                )
              )
            )
          )
          
          
       
          (setq main-countinue (getstring "Do you want to continue in mainBar? [Y/N]"))
            (if (or (equal main-countinue "n") (equal main-countinue "N"))
            (setq em 0)
          )
        )
    )
    (progn
        (setq nameOfObject (strcase(getstring t "\nEnter name of object BEAM/COLUMN/FOUNDATION/SLAB/WALL [B/C/F/S/W]")))
          (while (not (member   nameOfObject   objects))
            (setq nameOfObject (strcase(getstring t " \nEnter name of object BEAM/COLUMN/FOUNDATION/SLAB/WALL [B/C/F/S/W]")))
          )
        (setq e1 1)
        (while ( > e1 0)
            (setq shapeE '("K" "S" "N" "NS" ))
            (setq shapes  (strcase(getstring t "choose the shape of elements khamoot(k)/135*Sanjaghi(S)/90*Sanjaghi(N)/90*135Sanjaghi(s3) [K/S/N/NS] ")))
            (while (not (member   shapes   shapeE))
            (setq shapes (strcase(getstring t "choose the shape of elements khamoot(k)/135*Sanjaghi(S)/90*Sanjaghi(N)/90*135Sanjaghi(s3) [K/S/N/NS] ")))
            )
        
          (if (or (equal shapes "Khamoot")  (equal shapes "k") (equal shapes "K") (equal shapes "khamoot"))
            (progn
                
                (defun calculate-stirrup-length (lengths widths covers dia)
                (setq lengthsn (- lengths (* 2 covers) ))
                (setq widthsn (- widths (* 2 covers) ))
                (setq stirrup (+ (* 2 (+ widthsn lengthsn))(* ( / dia 1000) 6)) )
                )
            
              (setq diameters '("8" "10" "12" "14" "16" "18" "20" "22" "25" "28" "30" "32"))
              (setq dia (atoi(getstring t "\nSelect diameter [8/10/12/14/16/18/20/22/25/28/30/32]: ")))
              (while (not (member  (rtos dia 2 0)  diameters))
                (setq dia (getint "\nInvalid diameter. Enter again: "))
              )
              (initget (+ 1 2 4))
              (setq lengthsn (getdist "\nInsert Tool (m): "))
              (initget (+ 1 2 4))
              (setq widthsn (getdist "\nInsert ARZ (m): "))
              (initget (+ 1 2 4))
              (setq cover (getdist "\nInsert cover (m) "))
              (setq stirrup-length (calculate-stirrup-length lengthsn widthsn cover dia))
              (initget (+ 1 4))
              (setq qty (getreal "put number of rebars or enter [0]"))
                    (if 
                          (or(= qty 0) (equal qty "0"))
                      (progn
                        (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                        (eza)
                        (setq dist AddSum)
                        (initget (+ 1 2 4))
                        (setq space (getreal "\nEnter the spacing between rebars in cm: "))
                        ; Calculate the number of rebars
                        (setq cmspace (/ space 100 ) )
                        (setq num (1+ (fix (/ dist cmspace))))
                      )
                      (setq num  qty)
                    )
              
              (setq tot (* num stirrup-length))
              (setq unit-weight (/ (cdr (assoc dia '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
              (setq total-weight  (* tot unit-weight))
               (if (or(equal space "")(equal space nil)) (setq space 0))
              (setq position (find-max-number))
              (setq finaltext (strcat  nameOfObject "/"  (rtos position 2 0) "/" (rtos num 2 0) "/\U+00D8" (rtos dia 2 0) "@" (rtos space 2 0)"cm/" " L=" (rtos stirrup-length 2 2) "/" (rtos tot 2 2) "m/" (rtos total-weight 2 2) " kg/" shapes "\n" )) 
              (alert (strcat "this is string: " " "finaltext) )
                            
              (vl-load-com)
              (setq mspace (vla-get-modelspace 
              (vla-get-activedocument 
                                (vlax-get-acad-object))))
                              ;;(setq currlayer (vla-get-layer doc))
              (initget (+ 1 2 4))
              (setq apt (getpoint "\nInsertion Point: "))

              (setq LayerName "steels")
                            
              (setq ht (getreal "\nHeight : "))
               (if (or (equal ht nil)(equal ht "")) (setq ht 0.2)(setq ht ht))             ;;(vla-put-layer finaltext  "steels")
              (command-s "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
              (setq thetext (vla-AddText mspace finaltext 
                                                (vlax-3d-point apt) ht))
              (vla-put-layer thetext LayerName )  ; Diameter of the rebar in cm
              
              (princ (strcat "\nStirrup Length: " (rtos stirrup-length 2 2) " m\n"))
              
              (setq prevX (car apt))
                          (setq prevY (cadr apt))
                          (setq prevZ (caddr apt))

                          ; Set the offset distance for the new text below the previous text
                          (setq offset (* ht -2.5))  ; Adjust this value as needed

                          ; Calculate the new Y-coordinate for the insertion point
                          (setq newY (+ prevY offset))

                          ; Create the new insertion point with the updated coordinates
                          (setq newApt (list prevX newY prevZ))
                        (setq blockname (strcat  nameOfObject   (rtos position 2 0)) )
                        (while (tblsearch "BLOCK" blockname)
                          (progn
                            (setq nnn 1)
                            (setq blockname "")
                            
                            (setq blockname (strcat  nameOfObject   (rtos position 2 0) "0" (itoa nnn) ) )
                            (setq nnn (1+ nnn))
                          )
                        )
              (create-stirrup-block newApt lengthsn widthsn ht blockname)
            )
            
          ) 
          (if (or (equal shapes "s") (equal shapes "S") )
            (progn
                
               (defun calculate-135*Sanjaghi-length ()
                 (initget (+ 1 2 4))
                (setq lengthsn (getdist "\nInsert Tool (m): "))
                (setq widthsn   (* ( / dia 1000) 6)  )
                (if (< widthsn 0.10) (setq widthsn 10))
                (setq total (+ (* 2 widthsn) lengthsn ))
                )

              (setq diameters '(8 10 12 14 16 18 20 22 25 28 30 32))
              (initget (+ 1 2 4))
              (setq dia (getreal "\nSelect diameter [8/10/12/14/16/18/20/22/25/28/30/32]: "))
              (while (not (member   dia   diameters))
                (setq dia (getreal "\nInvalid diameter. Enter again [8/10/12/14/16/18/20/22/25/28/30/32]: "))
              )
              (setq Sanjaghi35-length (calculate-135*Sanjaghi-length ))
              (initget (+ 1 4))
              (setq qty (getreal "put number of rebars or enter [0]"))
                    (if 
                          (or(= qty 0) (equal qty "0"))
                      (progn
                        (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                        (eza)
                        (setq dist AddSum)
                        (initget (+ 1 2 4))
                        (setq space (getreal "\nEnter the spacing between rebars in cm: "))
                        ; Calculate the number of rebars
                        (setq cmspace (/ space 100 ) )
                        (setq num (1+ (fix (/ dist cmspace))))
                      )
                      (setq num  qty)
                    )
              
              (setq tot (* num total))
              (setq dia1 (fix dia))
              (setq unit-weight (/ (cdr (assoc dia1 '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
              (setq total-weight  (* tot unit-weight))
               (if (or(equal space "")(equal space nil)) (setq space 0))
              (setq position (find-max-number))
              (setq finaltext (strcat  nameOfObject "/"  (rtos position 2 0) "/" (rtos num 2 0) "/\U+00D8" (rtos dia 2 0) "@" (rtos space 2 0)"cm/" " L=" (rtos Sanjaghi35-length 2 2) "/" (rtos tot 2 2) "m/" (rtos total-weight 2 2) " kg/" shapes "\n" )) 
              (alert (strcat "this is string: " " "finaltext) )
                            
              (vl-load-com)
              (setq mspace (vla-get-modelspace 
              (vla-get-activedocument 
                                (vlax-get-acad-object))))
                              ;;(setq currlayer (vla-get-layer doc))
              (initget (+ 1 2 4))
              (setq apt (getpoint "\nInsertion Point: "))

              (setq LayerName "steels")
                       
              (setq ht (getreal "\nHeight : "))
               (if (or (equal ht nil)(equal ht "")) (setq ht 0.2)(setq ht ht))             ;;(vla-put-layer finaltext  "steels")
              (command-s "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
              (setq thetext (vla-AddText mspace finaltext 
                                                (vlax-3d-point apt) ht))
              (vla-put-layer thetext LayerName )  ; Diameter of the rebar in cm
              
              (princ (strcat "\nStirrup Length: " (rtos Sanjaghi35-length 2 2) " m\n"))
              
              (setq prevX (car apt))
                          (setq prevY (cadr apt))
                          (setq prevZ (caddr apt))

                          ; Set the offset distance for the new text below the previous text
                          (setq offset (* ht -2.5))  ; Adjust this value as needed

                          ; Calculate the new Y-coordinate for the insertion point
                          (setq newY (+ prevY offset))

                          ; Create the new insertion point with the updated coordinates
                          (setq newApt (list prevX newY prevZ))
                        (setq blockname (strcat  nameOfObject   (rtos position 2 0)) )
                        (while (tblsearch "BLOCK" blockname)
                          (progn
                            (setq nnn 1)
                            (setq blockname "")
                            
                            (setq blockname (strcat  nameOfObject   (rtos position 2 0) "0" (itoa nnn) ) )
                            (setq nnn (1+ nnn))
                          )
                        )
              (create-135*Sanjaghi-block newApt lengthsn widthsn ht blockname)
              )
            
          ) 
          (if (or (equal shapes "n") (equal shapes "N") )
            (progn
                
                (defun calculate-90*Sanjaghi-length ()
                (initget (+ 1 2 4))
                (setq lengthsn (getdist "\nInsert Tool (m): "))
                (setq widthsn   (* ( / dia 1000) 6)  )
                (if (< widthsn 0.10) (setq widthsn 0.1))
                (setq total (+ (* 2 widthsn) lengthsn ))
                  
                )
            
              (setq diameters '(8 10 12 14 16 18 20 22 25 28 30 32))
              (setq dia (getreal "\nSelect diameter [8/10/12/14/16/18/20/22/25/28/30/32]: "))
              (while (not (member   dia   diameters))
                (setq dia (getreal "\nInvalid diameter. Enter again [8/10/12/14/16/18/20/22/25/28/30/32]: "))
              )
              (setq Sanjaghi35-length (calculate-90*Sanjaghi-length ))
              (initget (+ 1 4))
              (setq qty (getreal "put number of rebars or enter [0]"))
                    (if 
                          (or(= qty 0) (equal qty "0"))
                      (progn
                        (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                        (eza)
                        (setq dist AddSum)
                         (initget (+ 1 2 4))
                        (setq space (getreal "\nEnter the spacing between rebars in cm: "))
                        ; Calculate the number of rebars
                        (setq cmspace (/ space 100 ) )
                        (setq num (1+ (fix (/ dist cmspace))))
                      )
                      (setq num  qty)
                    )
              
              (setq tot (* num total))
              (setq dia1 (fix dia))
              (setq unit-weight (/ (cdr (assoc dia1 '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
              (setq total-weight  (* tot unit-weight))
               (if (or(equal space "")(equal space nil)) (setq space 0))
              (setq position (find-max-number))
              (setq finaltext (strcat  nameOfObject "/"  (rtos position 2 0) "/" (rtos num 2 0) "/\U+00D8" (rtos dia 2 0) "@" (rtos space 2 0)"cm/" " L=" (rtos Sanjaghi35-length 2 2) "/" (rtos tot 2 2) "m/" (rtos total-weight 2 2) " kg/" shapes "\n" )) 
              (alert (strcat "this is string: " " "finaltext) )
                            
              (vl-load-com)
              (setq mspace (vla-get-modelspace 
              (vla-get-activedocument 
                                (vlax-get-acad-object))))
                              ;;(setq currlayer (vla-get-layer doc))
              (initget (+ 1 2 4))
              (setq apt (getpoint "\nInsertion Point: "))

              (setq LayerName "steels")
                         
              (setq ht (getreal "\nHeight : "))
               (if (or (equal ht nil)(equal ht "")) (setq ht 0.2)(setq ht ht))             ;;(vla-put-layer finaltext  "steels")
              (command-s "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
              (setq thetext (vla-AddText mspace finaltext 
                                                (vlax-3d-point apt) ht))
              (vla-put-layer thetext LayerName )  ; Diameter of the rebar in cm
              
              (princ (strcat "\nStirrup Length: " (rtos total 2 2) " m\n"))
              
              (setq prevX (car apt))
                          (setq prevY (cadr apt))
                          (setq prevZ (caddr apt))

                          ; Set the offset distance for the new text below the previous text
                          (setq offset (* ht -3))  ; Adjust this value as needed

                          ; Calculate the new Y-coordinate for the insertion point
                          (setq newY (+ prevY offset))

                          ; Create the new insertion point with the updated coordinates
                          (setq newApt (list prevX newY prevZ))
                        (setq blockname (strcat  nameOfObject   (rtos position 2 0)) )
                        (while (tblsearch "BLOCK" blockname)
                          (progn
                            (setq nnn 1)
                            (setq blockname "")
                            
                            (setq blockname (strcat  nameOfObject   (rtos position 2 0) "0" (itoa nnn) ) )
                            (setq nnn (1+ nnn))
                          )
                        )
              (setq widthsn   (* ( / dia 1000) 6)  )
              (if (< widthsn 0.10) (setq widthsn 0.1))
              (create-90*Sanjaghi-block newApt lengthsn widthsn ht blockname)
            )
            
          )
          (if (or (equal shapes "ns") (equal shapes "NS") )
            (progn
                
                (defun calculate-90*135Sanjaghi-length ()
                (initget (+ 1 2 4))
                (setq lengthsn (getdist "\nInsert Tool (m): "))
                (setq widthsn   (* ( / dia 1000) 6)  )
                (if (< widthsn 0.10) (setq widthsn 0.1))
                (setq total (+ (* 2 widthsn) lengthsn ))
                  
                )
            
              (setq diameters '(8 10 12 14 16 18 20 22 25 28 30 32))
              (setq dia (getreal "\nSelect diameter [8/10/12/14/16/18/20/22/25/28/30/32]: "))
              (while (not (member   dia   diameters))
                (setq dia (getreal "\nInvalid diameter. Enter again [8/10/12/14/16/18/20/22/25/28/30/32]: "))
              )
              (setq Sanjaghi9035-length (calculate-90*135Sanjaghi-length ))
              (initget (+ 1 4))
              (setq qty (getreal "put number of rebars or enter [0]"))
                    (if 
                          (or(= qty 0) (equal qty "0"))
                      (progn
                        (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                        (eza)
                        (setq dist AddSum)
                         (initget (+ 1 2 4))
                        (setq space (getreal "\nEnter the spacing between rebars in cm: "))
                        ; Calculate the number of rebars
                        (setq cmspace (/ space 100 ) )
                        (setq num (1+ (fix (/ dist cmspace))))
                      )
                      (setq num  qty)
                    )
              
              (setq tot (* num total))
              (setq dia1 (fix dia))
              (setq unit-weight (/ (cdr (assoc dia1 '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
              (setq total-weight  (* tot unit-weight))
               (if (or(equal space "")(equal space nil)) (setq space 0))
              (setq position (find-max-number))
              (setq finaltext (strcat  nameOfObject "/"  (rtos position 2 0) "/" (rtos num 2 0) "/\U+00D8" (rtos dia 2 0) "@" (rtos space 2 0)"cm/" " L=" (rtos Sanjaghi9035-length 2 2) "/" (rtos tot 2 2) "m/" (rtos total-weight 2 2) " kg/" shapes "\n" )) 
              (alert (strcat "this is string: " " "finaltext) )
                            
              (vl-load-com)
              (setq mspace (vla-get-modelspace 
              (vla-get-activedocument 
                                (vlax-get-acad-object))))
                              ;;(setq currlayer (vla-get-layer doc))
              (initget (+ 1 2 4))
              (setq apt (getpoint "\nInsertion Point: "))

              (setq LayerName "steels")
                         
              (setq ht (getreal "\nHeight : "))
               (if (or (equal ht nil)(equal ht "")) (setq ht 0.2)(setq ht ht))             ;;(vla-put-layer finaltext  "steels")
              (command-s "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
              (setq thetext (vla-AddText mspace finaltext 
                                                (vlax-3d-point apt) ht))
              (vla-put-layer thetext LayerName )  ; Diameter of the rebar in cm
              
              (princ (strcat "\nSanjaghi Length: " (rtos total 2 2) " m\n"))
              
              (setq prevX (car apt))
                          (setq prevY (cadr apt))
                          (setq prevZ (caddr apt))

                          ; Set the offset distance for the new text below the previous text
                          (setq offset (* ht -3))  ; Adjust this value as needed

                          ; Calculate the new Y-coordinate for the insertion point
                          (setq newY (+ prevY offset))

                          ; Create the new insertion point with the updated coordinates
                          (setq newApt (list prevX newY prevZ))
                        (setq blockname (strcat  nameOfObject   (rtos position 2 0)) )
                        (while (tblsearch "BLOCK" blockname)
                          (progn
                            (setq nnn 1)
                            (setq blockname "")
                            
                            (setq blockname (strcat  nameOfObject   (rtos position 2 0) "0" (itoa nnn) ) )
                            (setq nnn (1+ nnn))
                          )
                        )
              (setq widthsn   (* ( / dia 1000) 6)  )
              (if (< widthsn 0.10) (setq widthsn 0.1))
              (create-90*135Sanjaghi-block newApt lengthsn widthsn ht blockname)
            )
            
          )
          
            (setq khamooot-countinue (getstring "Do you want to continue? [Y/N]"))
            (if (or (equal khamooot-countinue "n") (equal khamooot-countinue "N"))
            (setq e1 0)
            )
        )
      
    )

  )
)
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
  (defun create-layer (layer-name)
    (setq layer-table (vla-get-Layers (vla-get-ActiveDocument (vlax-get-acad-object))))
    ;;(setq layer-name (getstring "Give the layer name: "))
    (setq layer (tblsearch "layer" layer-name))
    (if (not layer)
      (progn
        (setq layers (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
        (setq layer (vla-add layers layer-name))
        (setq color (getstring t "Enter the color for layer [Red/Yellow/Green/Cyan/Blue/Magnet/White]: "))
        (if (or(equal color "r" )(equal color "R" )) (vla-put-Color layer acRed) )
        (if (or(equal color "Y" )(equal color "y" )) (vla-put-Color layer acYellow) )
        (if (or(equal color "G" )(equal color "g" )) (vla-put-Color layer acGreen) )
        (if (or(equal color "C" )(equal color "c" )) (vla-put-Color layer acCyan) )
        (if (or(equal color "B" )(equal color "b" )) (vla-put-Color layer acBlue) )
        (if (or(equal color "M" )(equal color "m" )) (vla-put-Color layer acMagenta) )
        (if (or(equal color "W" )(equal color "w" )) (vla-put-Color layer acMagenta) ))) layer)
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================

(defun C:elu ()
  (setvar "LUNITS" 2) ; Set units mode to decimal
  (create-layer "steels")
  (create-layer "blocks1285785")
  (setq j "Hello")           
  (while ( equal j "Hello") 
    (setq porsesh (strcase(getstring t"POSE or TABLE? [P/T]")))
    (if (equal porsesh "P")(put-text))
    (if (equal porsesh "T")(tbl))
    
  ;; Prompt the user if they want to repeat
  (setq repeat-prompt (getstring t "\nDo you want to [Enter] another length? or exit [e]"))
    (if(or (equal repeat-prompt "E") (equal repeat-prompt "e"))
       (progn
        (setq j "Goodbye")
        (prompt "Have a Good Time!❀\n") 
       )
    )
      )    
  )
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
