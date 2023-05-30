(defun EZA ( )

        (defun myerror (msg)
          (setvar 'lunits *lunits)
          (setq *error* myerror)
          )
          (princ "Pres Enter after finish")
          (setq *error* myerror)
          
          (setq *lunits (getvar 'lunits))
          (setvar 'lunits 4)  ;to accept any input format, incl scientific and arch.
          (if (numberp addsum)
              (progn
                (princ (strcat"\nEnter a number or distance, or [Enter] to start with " (rtos AddSum) ": "))
                (princ (strcat "\n= " (rtos 0 *lunits) " + "))
                (initget 32) ; null, 0, and negatives permitted
                (setq value (getdist))
                (if (/= value nil)(setq addSum value))
                )
              (progn
                (princ  "\nEnter a number or specify a distance: ")
                (setq addSum 0)
                )
            )

          (setq value 0)
          (while (/= value nil)
            (princ (strcat "\n= " (rtos AddSum *lunits) " + "))
            (initget 32)
            (setq value (getdist))
            (if value
              (progn
                (setq AddSum (+ AddSum value))
                )
              )
            );while

          (setvar 'lunits *lunits)
          (princ (strcat "\nAddSum = " (rtos AddSum *lunits) "  \[" (rtos AddSum 2 8) "]"))
          (princ)
);defun

;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
(defun sbe ()
      (setvar "cmdecho" 0)

      (defun getArc (en)
        (command-s "lengthen" en "")
        (getvar "perimeter")
      )

      (defun getLine (en)
        (setq enlist (entget en))
        (distance (cdr (assoc 10 enlist)) (cdr (assoc 11 enlist)))
      )

      (defun getPoly (en)
        (command "area" "Object" en)
        (getvar "perimeter")
      )
    (setq entekhab (getstring t "Do you want put dist(d) or select shapes(s) [d/s]"))
      (if (or (equal entekhab "s")(equal entekhab "S"))
        (progn
        (if (setq eset (ssget))
            (progn
                (setq totalLen 0)
                (setq cntr 0)
                (while (< cntr (sslength eset))
                  (setq en (ssname eset cntr))
                  (setq enlist (entget en))
                  (setq enType (cdr (assoc 0 enlist)))
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
                  (while (< (strlen enType) 12) (setq enType (strcat enType " ")))
                  
                  (princ enType)
                  (princ "\n Found ")
                  (princ " with a length of: ")
                  (princ (rtos len))
                  (setq totalLen (+ totalLen len))
                  (setq cntr (+ cntr 1))
                )
              )
          )
        )
        (progn
         (princ "put dist:")
        (EZA)
        (setq totalLen AddSum)
        )
       
                    
      )

      (setvar "cmdecho" 1)
      (setq cntr (if cntr cntr 0))
      (alert (strcat "\n Found " (itoa cntr) " entitie(s) with a Total Length of " (rtos totalLen)))
      (princ)
)
;;==========================================================================================================
;;==========================================================================================================
(defun calculate-pose-sums (sel)
  
  (setq contents "")
  (setq pos '())
  (if sel
    (progn
      (setq nents (sslength sel))
      (setq i 0)
      (while (< i nents)
        (setq ent (ssname sel i))
        (setq pos (cons (vla-get-TextString (vlax-ename->vla-object ent)) pos))
        (setq i (1+ i))))
    (prompt "\nNo text found in layer 'pg'."))
  

  (setq poseRebars (list))
  (foreach str pos
      (setq s (vl-string-position (ascii "=") str))
    (setq r (vl-string-position (ascii "/") str))
    (setq kk (vl-string-position (ascii "k") str))
    (setq pose (substr str 1 r))
    (setq quantity (substr str  (+ 3 s) (- kk 3)))
    (setq nn (vl-string-position (ascii "k") quantity))
    (setq quantity (atof (substr quantity  1 nn)))
    (setq ad (cons pose quantity))
    (setq poseRebars (cons ad poseRebars)))
  
  (setq poseSums '())
  (foreach item poseRebars
    (setq pose (car item)
          quantity (cdr item))
    (setq existingPose (assoc pose poseSums))
    (if existingPose
      (setq existingQuantity (cdr existingPose)
            newQuantity (+ existingQuantity quantity))
      (setq newQuantity quantity))
    (setq poseSums (cons (cons pose newQuantity) (vl-remove (lambda (x) (equal pose (car x))) poseSums))))
  
  (setq maxPoseSums '())
  (foreach item poseSums
    (setq pose (car item)
          quantity (cdr item))
    (setq existingPose (assoc pose maxPoseSums))
    (if existingPose
      (setq existingQuantity (cdr existingPose)
            newQuantity (max existingQuantity quantity))
      (setq newQuantity quantity))
    (setq maxPoseSums (cons (cons pose newQuantity) (vl-remove (lambda (x) (equal pose (car x))) maxPoseSums))))
  
  (setq poslist '())
  (foreach item maxPoseSums
    (if (not (assoc (car item) poslist))
      (setq poslist (cons item poslist))))
  
  
  poslist)
;;==================================================================================================================
;;==================================================================================================================
(defun c:pg ()
    (create-layer"pg")
    (setq k 0)
    (setq count 0)
    (while (= k 0)
        (prompt (strcat "PG" pgname ))
        (setq pgname (getstring "name of pg"))
        
        (setq length1(getdist "length: ") )
        (setq width (getdist "width: "))
        (setq weight 7850)
        (setq totalw (* (/ length1 100) (/ width 100) weight ))
        
        (setq text1 (strcat "PG" pgname "/" "(PL/"  (rtos(/ length1 100) 2 4 )  "*" (rtos(/ width 100)2 4 ) ") " " * " (rtos weight 2 0 ) " = " (rtos totalw 2 2) " kg" ))
        (alert (strcat "this is string: " " "text1) )
         ;;(setq currlayer (vla-get-layer doc))
       
        (vl-load-com)
        (setq mspace (vla-get-modelspace 
                          (vla-get-activedocument 
                                (vlax-get-acad-object))))
          ;;(setq currlayer (vla-get-layer doc))
          (princ count)
          (if (> count 0)
            (progn
              (prompt (strcat "PG" pgname ))
               (setq prevX (car apt))
                (setq prevY (cadr apt))
                (setq prevZ (caddr apt))

              ; Set the offset distance for the new text below the previous text
              (setq offset (* (* -1 count) (* 2 ht)))  ; Adjust this value as needed

              ; Calculate the new Y-coordinate for the insertion point
              (setq newY (+ prevY offset))

              ; Create the new insertion point with the updated coordinates
              (setq newApt (list prevX newY prevZ))           
              (setq LayerName "pg")
          ;;(vla-put-layer finaltext  "steels")
            (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
            (setq thetext (vla-AddText mspace text1 
                                          (vlax-3d-point newApt) ht))
            (vla-put-layer thetext LayerName )
            )
            (progn
              (setq apt (getpoint "\nInsertion Point: "))

              (setq LayerName "pg")
              
              (setq ht (getreal "\nHeight : "))
            ;;(vla-put-layer finaltext  "steels")
              (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
              (setq thetext (vla-AddText mspace text1 
                                            (vlax-3d-point apt) ht))
              (vla-put-layer thetext LayerName )
            )
           
         )
         
       
    
        (setq qua (getstring "\nContinue? [y/n]"))
        (if (or (equal qua "y")(equal qua "Y") (equal qua "(Y"))
          (progn
           (setq count (1+ count))
          ;;(alert (strcat " count is: " (rtos count 2 2)))
          )
         
        
        )
        (if (or (equal qua "n")(equal qua "N"))
        (setq k 1)
        
        )     
    ) 
)
;;==================================================================================================================
;;==================================================================================================================

(defun c:putsums()
  (setq sel (ssget "_X" '((0 . "TEXT")(8 . "pg"))))
  (setq result (calculate-pose-sums sel))
  (setq apt (getpoint "\nInsertion Point: "))
  (setq ht (getreal "\nHeight : "))
  (setq count 0)
  (vl-load-com)
        (setq mspace (vla-get-modelspace 
                          (vla-get-activedocument 
                                (vlax-get-acad-object))))
  (foreach pg result
    (setq pose (car pg)
        quantity (cdr pg))
    (setq count (1+ count))
    (setq textsum (strcat  pose ":" (vl-princ-to-string quantity) ))
                (setq prevX (car apt))
                (setq prevY (cadr apt))
                (setq prevZ (caddr apt))

              ; Set the offset distance for the new text below the previous text
              (setq offset (* (* -1 count) (* 2 ht)))  ; Adjust this value as needed

              ; Calculate the new Y-coordinate for the insertion point
              (setq newY (+ prevY offset))

              ; Create the new insertion point with the updated coordinates
              (setq newApt (list prevX newY prevZ))           
              (setq LayerName "pg")
          ;;(vla-put-layer finaltext  "steels")
            (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
            (setq thetext (vla-AddText mspace textsum 
                                          (vlax-3d-point newApt) ht))
            (vla-put-layer thetext LayerName )
  
  
  )
)
;;==================================================================================================================
;;==================================================================================================================
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
      (if (or(equal color "W" )(equal color "w" )) (vla-put-Color layer acMagenta) )
      )
      (prompt (strcat "layer '"layer-name"' is created and it's color is '"color"'"))
      ) layer)
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
(defun c:piplen ()
  (setq layert (getstring "Give the Name of Layer Or default is <pglength>:  "))
  (if (or (equal layert nil) (equal layert ""))
    (setq layer "pglength")
     (create-layer layert)
  )
  (if (not(equal layert nil))
     (create-layer layert)
  )
   
    (setq k 0)
    (setq count 0)
    (while (= k 0)
        
        (setq pgname (getstring "name of pg"))
      (prompt (strcat "PG" pgname ))
        (setq shape (getstring "do you want to shape or Bricke? [s/b]"))
        (if  (or (equal shape "s")(equal shape "S"))
        (command-s "_arc"))
        (if  (or (equal shape "b")(equal shape "B"))
          (progn
            (setq brea 0)
          (while (= brea 0)
             (command-s "_BREAK")
            (setq bre (getstring "continue? [y/n]"))
             (if  (or (equal bre "n")(equal bre "N"))
               (setq brea 1))
          )
          )
       )
        (sbe)
        (setq leng totalLen)
        
        (setq text1 (strcat "PG" pgname "/"  (rtos(/ leng 10) 2 4 )  ))
        (prompt (strcat "this is string: " " "text1) )
         ;;(setq currlayer (vla-get-layer doc))
       
    
              (setq apt (getpoint "\nInsertion Point: "))

              (setq LayerName layert)
              (vl-load-com)
              (setq mspace (vla-get-modelspace 
                                (vla-get-activedocument 
                                      (vlax-get-acad-object))))
              (setq ht (getreal "\nHeight : "))
              (if (equal ht nil) (setq ht 1))
              ;;(command-s "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
              (setq thetext (vla-AddText mspace text1 
                                            (vlax-3d-point apt) ht))
              (vla-put-layer thetext LayerName )
           
           

         
       
    
        (setq qua (getstring "\nContinue? [y/n]"))
       
        (if (or (equal qua "n")(equal qua "N"))
        (setq k 1)
        
        )
      
      
      
    )  
)




