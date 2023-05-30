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
(defun calculate (num1  operator num2 )

    (if(equal operator "+") (+ num1 num2))
    (if (equal operator "-") (- num1 num2))
    (if (equal operator "*") (* num1 num2))
    (if (equal operator "/") (/ num1 num2))
    )
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
(defun sbe ()
      (setvar "cmdecho" 0)

      (defun getArc (en)
        (command "lengthen" en "")
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
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================

(defun split-string-to-lines (str)
  (vl-load-com)
  (setq lines '())
  (setq line "")
  (setq i 1)
  (setq leng (strlen str))
  (while (< (- i 1) leng)
    (setq ch (substr str i 1))
    (if (equal ch "\n")
      (progn
        (setq lines (cons line lines))
        (setq line ""))
      (setq line (strcat line ch)))
    (setq i (+ i 1)))
  (setq lines (cons line lines))
  (reverse lines))
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================

(defun ACAD2XL (XLTEXT /)
        (vl-load-com) ;load extended AutoLISP functions (must be run prior to VL functions)
        ;get excel application
        (setq XLapp (vlax-get-or-create-object "Excel.Application")
        XLrun (vlax-get-property XLapp 'Visible)
        )
        ;check if excel running
        (if (= XLrun :vlax-false)
        ;excel is not running
        (progn
        (vlax-release-object XLapp)
        (setq XLapp nil XLrun nil)
        (gc)
        (alert "\nERROR - Please Start Excel and select Cell (to place data)
        prior to running this command.")
        )
        ;excel is running
        (progn
        (setq XLwbk (vlax-get-property XLapp 'WorkBooks)
        XLash (vlax-get-property XLapp 'ActiveSheet)
        XLcel (vlax-get-property XLapp 'ActiveCell)
        )
        ;check if workbook is active
        (if (and XLash )
        ;excel workbook is active
        (progn
        (setq XLcol (vlax-get-property XLcel 'Column)
        XLrow (vlax-get-property XLcel 'Row)
        XLcva (vlax-get-property XLcel 'Text)
        XLtxt (vlax-variant-value XLcva)
        )
        ;convert column and row number into Cell text (Example - Column 1,Row 1 = A1

        (if (= (/ XLcol 26) 0)
        (setq TC (strcat (chr (+ (rem XLcol 26) 64)) (itoa XLrow)))
        (setq TC (strcat (chr (+ (/ XLcol 26) 64)) (chr (+ (rem XLcol
        26) 64)) (itoa XLrow)))
        )
        

        (setq CELL (strcat  TC":" TC  ))
          ;;(setq CELL1 (strcat (itoa 1)":" (itoa 1)))
        ;check if cell all ready has data
        (if (= XLtxt "")
        (setq YN "Y")
        (setq YN (strcase (getstring (strcat "\nNOTICE - Excel Cell
        contains [" XLtxt "] - Change [n]=No or [Y]=Yes ? "))))
        )
        (if (member YN (list "" "Y"))
        (progn
          
        (setq Cells (vlax-get-property XLash 'Range CELL))
        ;;(setq del vlax-invoke-method Cells 'ClearContents)
        ;;(vlax-invoke-method Cells "Selection.ClearContents")
        (setq VTXT (vlax-make-variant XLTEXT vlax-vbString))
          
        (vlax-put-property Cells 'Value2 VTXT)
        ) 
          
          
        )
        ;release objects
        (vlax-release-object XLash)
        (vlax-release-object XLwbk)
        (vlax-release-object XLapp)
        (setq XLapp nil XLrun nil XLwbk nil XLash nil XLcel nil Cells nil)
        (gc)
          |;
        )
        ;excel is running, but no active workbooks
        (progn
        (alert "\nERROR - Excel is running, but there are NO workbooks
        open.")
        (vlax-release-object XLwbk)
        (vlax-release-object XLapp)
        (setq XLapp nil XLrun nil XLwbk nil)
        )
        )
        )
        )
        (princ)
        )


        (vl-load-com)

        (defun c:openExcel (/ path filename)
          (if (findfile (setq path (vl-string-right-trim "\\" (getvar 'dwgprefix))
                              path (substr path 1 (vl-string-position (ascii "\\") path 0 T))
                              filename (strcat "\"" path "\\QCL Issue comment.xlsm\"")))
            (startapp "C:\\Program Files\\Microsoft Office\\root\Office16\\EXCEL.EXE" filename )
            (princ "\n'QCL Issue Register' File not found."))
            (princ)
  ) 

;;=======================================================================================================================
;;=======================================================================================================================
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
  
  (setq max-number (apply 'max pos))

  
  
  )
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
(defun export-to-excel ()
        (vl-load-com)
        (setq xl (vlax-get-object "Excel.Application"))
        
        (if xl 
          (progn
          (vlax-put-property xl 'visible :vlax-true)
        (setq workbooks (vlax-get-property xl "workbooks"))
        (setq address(getfiled "Select a excel File" "listo.xlsx" "xlsx" 8))
        
        (setq wb (vlax-invoke-method workbooks "Open" address))
        (setq sheets (vlax-get-property wb "Sheets"))
        (setq sheet (vlax-get-property sheets "Item" 1))
        (vlax-put-property sheet "Name" "Data")
        (setq row 1 col 1)
        (setq sel (ssget "_X" '((0 . "TEXT")(8 . "steels"))))
        (setq contents "")
        (if sel
          (progn
            (setq nents (sslength sel))
            (setq i 0)
            (while (< i nents)
              (setq ent (ssname sel i))
              (setq contents (strcat contents (vla-get-TextString (vlax-ename->vla-object ent)) "\n"))
              (setq contents (substr contents 1 (1- (strlen contents)))) ; Remove the last newline character
              ;;(alert contents)
              (setq i (1+ i))))
          (prompt "\nNo text found in layer 'steels'."))
      ;; remove extra empty line
      (setq lines (split-string-to-lines contents))
        ;;
        (vl-load-com)
      ; ask for file path
      (setq file_path (getfiled "Save CSV File" "" "csv" 1))

      ; create file
      (setq file (open file_path "w"))
      (write-line "name, Pos,Part Count, Size, @POS, Length,Total lengths(m), Total Weight(kg)" file)
      ; write contents to file
        
      (setq lines (split-string-to-lines contents))

      (foreach line lines
        
        (write-line line file)
      )

      ; close file
      (close file)

      ; release objects
      (vlax-release-object sheet)
      (vlax-release-object sheets)
      (vlax-release-object wb)
      (vlax-release-object workbooks)
      (vlax-release-object xl)


      
        
        (foreach line lines
          (princ line)
        (ACAD2XL line)
        (alert (strcat "Exported " (itoa (strlen line)) " line to Excel successfully."))
      ) 
        

        
        )
        (progn
        (setq sel (ssget "_X" '((0 . "TEXT")(8 . "steels"))))
        (setq contents "")
        (if sel
          (progn
            (setq nents (sslength sel))
            (setq i 0)
            (while (< i nents)
              (setq ent (ssname sel i))
              (setq contents (strcat contents (vla-get-TextString (vlax-ename->vla-object ent)) "\n"))
              (setq contents (substr contents 1 (1- (strlen contents)))) ; Remove the last newline character
              ;;(alert contents)
              (setq i (1+ i))))
          (prompt "\nNo text found in layer 'steels'."))
      ;; remove extra empty line


      (setq lines (split-string-to-lines contents))
          (defun sort-by-number-after-star (list)
        (vl-sort-i list
                  (lambda (a b)
                    (let* ((num-a (nth 1 (vl-string-split a "*")))
                            (num-b (nth 1 (vl-string-split b "*"))))
                      (< (atoi num-a) (atoi num-b))))))
        ;;
        (vl-load-com)
      ; ask for file path
      (setq file_path (getfiled "Save CSV File" "" "csv" 1))

      ; create file
      (setq file (open file_path "w"))
      (write-line "name, Pos,Part Count, Size, @POS, Length,Total lengths(m), Total Weight(kg)" file)
      ; write contents to file
        
      (setq lines (split-string-to-lines contents))

      (foreach line lines
        
        (write-line line file)
      )

      ; close file
      (close file)

        
        )
  
      )
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
                              
                        (setq beam (getstring  t "\nIs it top or bottom of the beam? [T/B]"))  
                      (if (or (equal beam "T" ) (equal beam "t" ))
                        (progn
                 
                          (setq overlap-length topB)
                        )
                        (progn
                     
                          (setq overlap-length otherB)
                        )
                      )
                            )
                            (if (or (equal nameOfObject "f") (equal nameOfObject "F") )
                              
                        (setq foundation (getstring t  "\nIs it top or bottom of the foundation [T/B]"))  
                      (if (or (equal foundation "T" ) (equal foundation "t" ))
                        (progn
                   
                          (setq overlap-length topF)
                        )
                        (progn
                      
                        (setq overlap-length botF)
                        
                        )
                      )
                            )
                            (if (or (equal nameOfObject "s") (equal nameOfObject "S") )
                              
                        (setq slab (getstring  t "\nIs it top or bottom of the Slab [T/B]"))  
                      (if (or (equal slab "T" ) (equal slab "t" ))
                        (progn
                  
                          (setq overlap-length topF)
                        )
                        (progn
                    
                          (setq overlap-length botF)
                        )
                      )
                            )
                            
                            (if (or (equal nameOfObject "w") (equal nameOfObject "W") )
                              
                        (setq wall (getstring t "\nIs it outside or inside of the wall? [O/I]"))  
                      (if (or (equal wall "O" ) (equal wall "o" ))
                        (progn
                   
                          
                          (setq overlap-length topB)
                        )
                        (progn
                    
                          (setq overlap-length otherB)
                        )
                      )
                            )
                            (if (equal nameOfObject "COLUMN")
                          
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
  (setq offset -2)
  (setq newY (+ prevY offset))
  (setq minVal (min (/ aa 10) (/ tt 10)))
  (setq nx (+ prevX minVal))
  (setq newstartX (list nx prevY prevZ))
  (setq ny (- prevY minVal))
  (setq newstartY (list prevX ny prevZ))
  (setq newApt (list prevX newY prevZ))
  (command-s "._rectangle" "Fillet" minVal apt "Dimensions" tt aa newApt)
  (setq rectangle (entlast))
  (setq newEndX (list (+ nx (* aa 0.35)) (- prevY (* tt 0.40)) prevZ))
  (setq newEndY (list (+ prevX (* aa 0.40)) (- ny (* tt 0.35)) prevZ))
  (command "._line" newstartX newEndX "")
  (setq linek1 (entlast))
  (command "._line" newstartY newEndY "")
  (setq linek2 (entlast))
  (setq LayerName "steels")

  (setq text1po (list (- prevX (* ht 1.5)) (- prevY (/ aa 2)) prevZ))
  (setq text2po (list (+ prevX (/ tt 2)) (+ prevY (/ ht 4)) prevZ))
  (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
  (setq text1 (rtos arz 2 2))
  (setq text2 (rtos tool 2 2))
  (setq thetext1 (vla-AddText mspace text1 (vlax-3d-point text1po) (/ 0.2 2)))
  (vla-put-layer thetext1 LayerName)
  (setq texts1 (entlast))
  (setq thetext2 (vla-AddText mspace text2 (vlax-3d-point text2po) (/ 0.2 2)))
  (vla-put-layer thetext2 LayerName)
  (setq texts2 (entlast))

  ; Assign the rectangle to a specific layer
  (vla-put-layer (vlax-ename->vla-object rectangle) LayerName)
  (vla-put-layer (vlax-ename->vla-object linek1) LayerName)
  (vla-put-layer (vlax-ename->vla-object linek2) LayerName)
  (vla-put-layer (vlax-ename->vla-object texts1) LayerName)
  (vla-put-layer (vlax-ename->vla-object texts2) LayerName)

  
    (while (tblsearch "BLOCK" blockname)
      (progn
        (setq blockname "")
        (prompt "\nPut a unic name on it: ")
        (setq blockname (getstring "Put a unic name on it: "))
      )
    ) |;
  
  (setq startss (list (+ prevX tt) (+ prevY (* 4 ht)) prevZ))
  (setq endss (list (- prevX (* ht 2.5)) (- prevY aa) prevZ))
  (setq blockpoints (ssget "W" startss endss))
  (command "._-block" blockname apt blockpoints "")
  (setq blockref (entlast))
  (vla-put-layer (vlax-ename->vla-object blockref) LayerName)
  (command "._insert" blockname apt "" "" "" )
  
  ; Set block visibility to 1 (visible)
  (vla-put-visible (vlax-ename->vla-object blockref) 1)

  )


;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================


(defun put-text ()
   (setq objects '("b" "c" "f" "s" "w" "B" "C" "F" "S" "W" ))
    (setq mainlist '("m" "p" "M" "P" ))
    
  (setq position (FIND-MAX-NUMBER))
  (setq mainrebar (getstring "\n main (M) Or part (P)[M/P]"))
   (while (not (member   mainrebar   mainlist))
    (setq mainrebar (getstring "\n main (M) Or part (P)[M/P]"))
   )
  
 
  
  
  (if (or (equal mainrebar "main") (equal mainrebar "m") (equal mainrebar "M") (equal mainrebar "Main") (equal mainrebar "MAIN"))
    (progn   
      
      (setq em 1)
        (while ( > em 0)
          (setq nameOfObject (getstring t "\nEnter name of object BEAM/COLUMN/FOUNDATION/SLAB/WALL [B/C/F/S/W]"))
  (while (not (member   nameOfObject   objects))
    (setq nameOfObject (getstring t " \nEnter name of object BEAM/COLUMN/FOUNDATION/SLAB/WALL [B/C/F/S/W]"))
  )
   (setq diameters '("8" "10" "12" "14" "16" "18" "20" "22" "25" "28" "30" "32"))
              (setq dia (atoi(getstring t "\nSelect diameter [8/10/12/14/16/18/20/22/25/28/30/32]: ")))
              (while (not (member  (rtos dia 2 0)  diameters))
                (setq dia (getint "\nInvalid diameter. Enter again: "))
              )
          (setq length-choice (getreal "\nEnter length of rebar or Enter [0] to select"))
      (if (= length-choice 0)
        (progn
              (sbe)
              (setq length-choice totalLen )
              (if (> length-choice 12)
                (progn
                  (setq over (getstring t "\nlength is bigger than 12 meters, calculate overlap [y/n] ?"))
                  (if
                    (or (equal over "y") (equal over "Y"))
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
                            (setq qty (getstring "put number of rebars or enter [a]"))
                            (if 
                                  (or(equal qty "a") (equal qty "A"))
                              (progn
                                (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                                (eza)
                                (setq dist AddSum)

                                (setq space (getreal "\nEnter the spacing between rebars in cm: "))
                                ; Calculate the number of rebars
                                (setq cmspace (/ space 100 ) )
                                (setq num (1+ (fix (/ dist cmspace))))
                              )
                              (setq num (atof qty))
                            )
                            (setq num (atof qty))
                            (setq tot (* num totalLen))
                            (setq unit-weight (/ (cdr (assoc dia '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
                            (setq total-weight  (* tot unit-weight))
                              (setq kham (* dia  12))
                              (setq khamm (/ kham 1000.))
                              (setq length-choice(- length-choice (- (- max-length khamm)  overlap-length )))
                              (setq remm(- length-choice (- max-length overlap-length)  ))
                              (while (> remm max-length )
                                    (setq number (1+ number ))
                                    (setq remm (- length-choice (- (* max-length number) (* overlap-length number))))
                              )
                              (setq remm1(+ remm khamm))
                              (setq length-choice (+ (* max-length  number)  remm1 max-length))
                            (alert (strcat "Overlap Length: " (rtos overlap-length 2 2 ) 
                                    " cm. Total count: " (rtos number 2 2 )  ". Number of 12 m rebars:" (rtos number 2 2 ) 
                                    ". Remaining total length: " (rtos remm 2 2)))
                          )
                          (progn
                              (setq remm(- length-choice (- max-length overlap-length)  ))
                              (while (> remm max-length )
                                    (setq number (1+ number ))
                                    (setq remm (- length-choice (- (* max-length number) (* overlap-length number))))
                              )
                            
                              (setq length-choice (+ (* max-length  number) remm))
                          )
                        )
                            
                        (princ (strcat "Overlap Length: " (rtos overlap-length 2 2 ) 
                                    " cm. Total count: " (rtos number 2 2 )  ". Number of 12 m rebars:" (rtos number 2 2 ) 
                                    ". Remaining total length: " (rtos remm 2 2)))  
                            (if (equal space nil) (setq space 0))
                  (setq position (find-max-number))
                      (setq tot (* max-length number num))
                   (setq unit-weight (/ (cdr (assoc dia '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
                            (setq total-weight  (* tot unit-weight))
                  
                  (setq finaltext (strcat  nameOfObject "/"  (rtos (+ position 1) 2 0) "/" (rtos (* num number) 2 0) "/"" \U+00D8" (rtos dia 2 0) "/" (rtos space 2 0) "/"" L=" (rtos max-length 2 2) "/" (rtos tot 2 2) "m/" (rtos total-weight 2 2) " kg\n" )) 
                  (alert (strcat "this is string: " " "finaltext) )
                      
                      (vl-load-com)
                      (setq mspace (vla-get-modelspace 
                                        (vla-get-activedocument 
                                              (vlax-get-acad-object))))
                        ;;(setq currlayer (vla-get-layer doc))
                        (setq apt (getpoint "\nInsertion Point: "))

                        (setq LayerName "steels")
                        
                        (setq ht (getreal "\nHeight : "))
                      ;;(vla-put-layer finaltext  "steels")
                        (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
                        (setq thetext (vla-AddText mspace finaltext 
                                                      (vlax-3d-point apt) ht))
                      (vla-put-layer thetext LayerName )
                      ;;ppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp
                       (if (equal space nil) (setq space 0))
                  (setq position (find-max-number))
                      (setq toto (* remm num))
                   (setq unit-weight (/ (cdr (assoc dia '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
                            (setq total-weight  (* toto unit-weight))
                  
                  (setq finaltextover (strcat  nameOfObject "/"  (rtos (+ position 1) 2 0) "/" (rtos num 2 0) "/"" \U+00D8" (rtos dia 2 0) "/" (rtos space 2 0) "/"" L=" (rtos remm 2 2) "/" (rtos toto 2 2) "m/" (rtos total-weight 2 2) " kg\n" )) 
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
                      ;;(vla-put-layer finaltext  "steels")
                        (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
                        (setq thetextover (vla-AddText mspace finaltextover 
                                                      (vlax-3d-point newApt) ht))
                      (vla-put-layer thetextover LayerName )
                    )
                    (setq length-choice totalLen ) 
                  )
                  
                )
                
              )
          (if (and (< length-choice 12) (= length-choice 12))
            (progn
                (setq kham (* dia  12))
                      (setq khamm (/ kham 1000.))
                      (alert  (strcat (rtos length-choice 2 2 ) "+ ((خم) 2 * " (rtos khamm 2 2 ) ")")   )
                      (setq totalLen (+ length-choice (* 2 khamm) ))

                    (setq qty (getstring "put number of rebars or enter [a]"))
                      (if 
                            (or(equal qty "a") (equal qty "A"))
                        (progn
                          (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                          (eza)
                          (setq dist AddSum)

                          (setq space (getreal "\nEnter the spacing between rebars in cm: "))
                          ; Calculate the number of rebars
                          (setq cmspace (/ space 100 ) )
                          (setq num (1+ (fix (/ dist cmspace))))
                        )
                        (setq num (atof qty))
                      )
                      (setq num (atof qty))
                      (setq tot (* num totalLen))
                      (setq unit-weight (/ (cdr (assoc dia '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
                      (setq total-weight  (* tot unit-weight)) 
                      (if (equal space nil) (setq space 0))
                      (setq position (find-max-number))
                    (setq finaltext (strcat  nameOfObject "/"  (rtos (+ position 1) 2 0) "/" (rtos num 2 0) "/"" \U+00D8 /" (rtos dia 2 0) "/" (rtos space 2 0) "/"" L=" (rtos totalLen 2 2) "/" (rtos tot 2 2) "m/" (rtos total-weight 2 2) " kg\n" ))
                      (alert (strcat "this is string: " " "finaltext) )
                        
                        (vl-load-com)
                        (setq mspace (vla-get-modelspace 
                                          (vla-get-activedocument 
                                                (vlax-get-acad-object))))
                          ;;(setq currlayer (vla-get-layer doc))
                          (setq apt (getpoint "\nInsertion Point: "))

                          (setq LayerName "steels")
                          
                          (setq ht (getreal "\nHeight : "))
                        ;;(vla-put-layer finaltext  "steels")
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
                  (setq over (getstring t "\nlength is bigger than 12 meters, calculate overlap [y/n] ?"))
                  (if
                    (or (equal over "y") (equal over "Y"))
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
                            (setq qty (getstring "put number of rebars or enter [a]"))
                            (if 
                                  (or(equal qty "a") (equal qty "A"))
                              (progn
                                (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                                (eza)
                                (setq dist AddSum)

                                (setq space (getreal "\nEnter the spacing between rebars in cm: "))
                                ; Calculate the number of rebars
                                (setq cmspace (/ space 100 ) )
                                (setq num (1+ (fix (/ dist cmspace))))
                              )
                              (setq num (atof qty))
                            )
                            (setq num (atof qty))
                           
                            
                            
                              (setq kham (* dia  12))
                              (setq khamm (/ kham 1000.))
                              (setq length-choice(- length-choice (- (- max-length khamm)  overlap-length )))
                              (setq remm(- length-choice (- max-length overlap-length)  ))
                              (while (> remm max-length )
                                    (setq number (1+ number ))
                                    (setq remm (- length-choice (- (* max-length number) (* overlap-length number))))
                              )
                              (setq remm(+ remm khamm))
                              (setq length-choice (+ (* max-length  number)  remm1 max-length))
                            (alert (strcat "Start rebar: " (rtos(- max-length khamm) 2 2 ) "\n+\nbend: " (rtos khamm 2 2 )
                                  "\nEnd rebar: " (rtos (- remm khamm) 2 2) "\n+\nbend: " (rtos khamm 2 2 )))
                          )
                          (progn
                              (setq remm(- length-choice (- max-length overlap-length)  ))
                              (while (> remm max-length )
                                    (setq number (1+ number ))
                                    (setq remm (- length-choice (- (* max-length number) (* overlap-length number))))
                              )
                            
                              (setq length-choice (+ (* max-length  number) remm))
                          )
                        )
                            
                        (princ (strcat "Overlap Length: " (rtos overlap-length 2 2 ) 
                                    " cm. Total count: " (rtos number 2 2 )  ". Number of 12 m rebars:" (rtos number 2 2 ) 
                                    ". Remaining total length: " (rtos remm 2 2)))  
                      ;;yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy
                       (if (equal space nil) (setq space 0))
                  (setq position (find-max-number))
                      (setq tot (* max-length number num))
                   (setq unit-weight (/ (cdr (assoc dia '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
                            (setq total-weight  (* tot unit-weight))
                  
                  (setq finaltext (strcat  nameOfObject "/"  (rtos (+ position 1) 2 0) "/" (rtos num 2 0) "/"" \U+00D8 /" (rtos dia 2 0) "/" (rtos space 2 0) "/"" L=" (rtos totalLen 2 2) "/" (rtos tot 2 2) "m/" (rtos total-weight 2 2) " kg\n" ))
                  (alert (strcat "this is string: " " "finaltext) )
                      
                      (vl-load-com)
                      (setq mspace (vla-get-modelspace 
                                        (vla-get-activedocument 
                                              (vlax-get-acad-object))))
                        ;;(setq currlayer (vla-get-layer doc))
                        (setq apt (getpoint "\nInsertion Point: "))

                        (setq LayerName "steels")
                        
                        (setq ht (getreal "\nHeight : "))
                      ;;(vla-put-layer finaltext  "steels")
                        (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
                        (setq thetext (vla-AddText mspace finaltext 
                                                      (vlax-3d-point apt) ht))
                      (vla-put-layer thetext LayerName )
                      ;;ppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp
                       (if (equal space nil) (setq space 0))
                  (setq position (find-max-number))
                      (setq toto (* remm num))
                   (setq unit-weight (/ (cdr (assoc dia '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
                            (setq total-weight  (* toto unit-weight))
                  
                  (setq finaltextover (strcat  nameOfObject "/"  (rtos (+ position 1) 2 0) "/" (rtos num 2 0) "/"" \U+00D8" (rtos dia 2 0) "/" (rtos space 2 0) "/"" L=" (rtos remm 2 2) "/" (rtos toto 2 2) "m/" (rtos total-weight 2 2) " kg\n" )) 
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
                      ;;(vla-put-layer finaltext  "steels")
                        (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
                        (setq thetextover (vla-AddText mspace finaltextover 
                                                      (vlax-3d-point newApt) ht))
                      (vla-put-layer thetextover LayerName )
                    )
                    (setq length-choice totalLen ) 
                  )
                )
             
          
          )
          (if (or (< length-choice 12) (= length-choice 12))
             (progn 
               (setq totalLen length-choice)
              (setq qty (getstring "put number of rebars or enter [a]"))
                            (if 
                                  (or(equal qty "a") (equal qty "A"))
                              (progn
                                (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                                (eza)
                                (setq dist AddSum)

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
                   (if (equal space nil) (setq space 0))
                  (setq position (find-max-number))
                  (setq finaltext (strcat  nameOfObject "/"  (rtos (+ position 1) 2 0) "/" (rtos num 2 0) "/"" \U+00D8 /" (rtos dia 2 0) "/" (rtos space 2 0) "/" " L=" (rtos totalLen 2 2) "/" (rtos tot 2 2) "m/" (rtos total-weight 2 2) " kg\n" )) 
                  (alert (strcat "this is string: " " "finaltext) )
                      
                      (vl-load-com)
                      (setq mspace (vla-get-modelspace 
                                        (vla-get-activedocument 
                                              (vlax-get-acad-object))))
                        ;;(setq currlayer (vla-get-layer doc))
                        (setq apt (getpoint "\nInsertion Point: "))

                        (setq LayerName "steels")
                        
                        (setq ht (getreal "\nHeight : "))
                      ;;(vla-put-layer finaltext  "steels")
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
        (setq shapeE '("k" "s1" "s2" "s3" ))
        (setq shapes (getstring "choose the shape of elements khamoot(k)/135*Sanjaghi(s1)/90*Sanjaghi(s2)/90*135Sanjaghi(s3) [k/s1/s2/s3] "))
        (while (not (member   shapes   shapeE))
        (setq shapes (getstring "choose the shape of elements khamoot(k)/135*Sanjaghi(s1)/90*Sanjaghi(s2)/90*135Sanjaghi(s3) [k/s1/s2/s3] "))
        )
        (setq e1 1)
        (while ( > e1 0)
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
              (setq lengthsn (getdist "\nInsert Tool: "))
              (setq widthsn (getdist "\nInsert ARZ: "))
              (setq cover (getdist "\nInsert cover "))
              (setq stirrup-length (calculate-stirrup-length lengthsn widthsn cover dia))
              
              (setq qty (getstring "put number of rebars or enter [a]"))
                    (if 
                          (or(equal qty "a") (equal qty "A"))
                      (progn
                        (princ "\nEnter the length of space in which the reinforcements are placed in m: ")
                        (eza)
                        (setq dist AddSum)

                        (setq space (getreal "\nEnter the spacing between rebars in cm: "))
                        ; Calculate the number of rebars
                        (setq cmspace (/ space 100 ) )
                        (setq num (1+ (fix (/ dist cmspace))))
                      )
                      (setq num (atof qty))
                    )
              
              (setq tot (* num stirrup-length))
              (setq unit-weight (/ (cdr (assoc dia '((8 . 0.617) (10 . 0.617) (12 . 0.888) (14 . 1.21) (16 . 1.58) (18 . 2.00) (20 . 2.47) (22 . 2.98) (25 . 3.85) (28 . 4.83) (30 . 5.55) (32 . 6.31))))))
              (setq total-weight  (* tot unit-weight))
               (if (equal space nil) (setq space 0))
              (setq finaltext (strcat  nameOfObject "/"  (rtos (+ position 1) 2 0) "/" (rtos num 2 0) "/"" \U+00D8" (rtos dia 2 0) "/" (rtos space 2 0)"/" " L=" (rtos stirrup-length 2 2) "/" (rtos tot 2 2) "m/" (rtos total-weight 2 2) " kg/" shapes "\n" )) 
              (alert (strcat "this is string: " " "finaltext) )
                            
              (vl-load-com)
              (setq mspace (vla-get-modelspace 
              (vla-get-activedocument 
                                (vlax-get-acad-object))))
                              ;;(setq currlayer (vla-get-layer doc))
              (setq apt (getpoint "\nInsertion Point: "))

              (setq LayerName "steels")
                              
              (setq ht (getreal "\nHeight : "))
                            ;;(vla-put-layer finaltext  "steels")
              (command "-style" "khayyam" "@Arial unicode MS" ht 0.85 0 "N" "N")
              (setq thetext (vla-AddText mspace finaltext 
                                                (vlax-3d-point apt) ht))
              (vla-put-layer thetext LayerName )  ; Diameter of the rebar in cm
              
              (princ (strcat "Stirrup Length: " (rtos stirrup-length 2 2) " m"))
              
              (setq prevX (car apt))
                          (setq prevY (cadr apt))
                          (setq prevZ (caddr apt))

                          ; Set the offset distance for the new text below the previous text
                          (setq offset -1.0)  ; Adjust this value as needed

                          ; Calculate the new Y-coordinate for the insertion point
                          (setq newY (+ prevY offset))

                          ; Create the new insertion point with the updated coordinates
                          (setq newApt (list prevX newY prevZ))
                        (setq blockname (strcat  nameOfObject   (rtos (+ position 1) 2 0)) )
                        (while (tblsearch "BLOCK" blockname)
                          (progn
                            (setq nnn 1)
                            (setq blockname "")
                            
                            (setq blockname (strcat  nameOfObject   (rtos (+ position 1) 2 0) "0" (itoa nnn) ) )
                            (setq nnn (1+ nnn))
                          )
                        )
              (create-stirrup-block newApt lengthsn widthsn ht blockname)
            )
            (put-text)
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
(defun insert-blocks-as-images-to-excel (blockname output-file)
  (setq xl (vlax-get-object "Excel.Application"))
  (setq wb (vlax-get-property xl "workbooks"))
  (setq sheet (vlax-invoke-method wb "ActiveSheet"))

  ;; Set the column headers
  (vlax-invoke-method sheet "Cells" 1 1 "Block Name")
  (vlax-invoke-method sheet "Cells" 1 2 "Image")

  (setq row 2) ; Start from row 2 for data

  ;; Iterate through the blocks and insert their images to Excel
  (foreach blk (vlax-invoke-method (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) "Item" blockname)
    (setq blkname (vla-get-name blk))
    (setq blkpoint (vlax-get-property blk 'insertionpoint))

    ;; Capture the block as an image
    (setq imagefile (strcat (vl-filename-base output-file) "_" (rtos row) ".png"))
    (command "-IMAGE" "EXPORT" "PATH" imagefile "Width" "500" "Height" "500" "Enter" blkpoint "")

    ;; Insert the image into Excel
    (vlax-invoke-method sheet "Cells" row 1 blkname)
    (vlax-invoke-method sheet "Pictures" "Insert" (getvar 'dwgprefix) imagefile (vlax-invoke-method sheet "Cells" row 2))

    (setq row (1+ row))
  )

  ;; Save and close the Excel file
  (vlax-invoke-method wb "SaveAs" output-file)
  (vlax-invoke-method wb "Close" :vlax-false)
  (vlax-release-object xl)
)
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================

(defun export-blocks-to-csv ()
  (setq file_path (getfiled "Save CSV File" "" "csv" 1))
  (setq csv-file (open file_path "w"))
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq mspace (vla-get-modelspace doc))
  (setq ss (ssget "_X" '((0 . "INSERT") (8 . "4"))))
  (if ss
    (progn
      (setq csv-content '("Block Name,Layer,X,Y,Z,Rotation Angle"))
      (setq nents (sslength ss))
      (setq i 0)
      (while (< i nents)
        (setq ent (ssname ss i))
        (if (= (vla-get-objectname (vlax-ename->vla-object ent)) "AcDbBlockReference")
          (progn
            (setq blk (vlax-ename->vla-object ent))
            (setq blockname (vla-get-name blk))
            (setq layer (vla-get-layer blk))
             (setq insertion-point (vlax-variant-value (vla-get-insertionpoint blk)))
            (setq x (vlax-safearray-get-element insertion-point 0))
                (setq y (vlax-safearray-get-element insertion-point 1))
                (setq z (vlax-safearray-get-element insertion-point 2))
            (setq rotation (vla-get-rotation blk))
            (setq csv-row (strcat blockname "," layer "," (rtos x) "," (rtos y) "," (rtos z) "," (rtos rotation)))
            (setq csv-content (append csv-content (list csv-row))))
          )
        (setq i (1+ i))
        )
      (setq csv-content (apply 'strcat (mapcar 'strcat csv-content '("\n"))))
      
      (vl-file-write csv-content csv-file)
      (prompt (strcat "\nCSV file exported: " csv-file))
      )
    (prompt "\nNo Block!")
    )
  )
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
(defun export-blocks-to-csv (csv-file)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq mspace (vla-get-modelspace doc))
  (setq ss (ssget "_X" '((0 . "INSERT") (8 . "4"))))
  (if ss
      (progn
        (setq csv-content '("Block Name,Image File"))
        (setq nents (sslength ss))
        (setq i 0)
        (while (< i nents)
          (setq ent (ssname ss i))
          (if (= (vla-get-objectname (vlax-ename->vla-object ent)) "AcDbBlockReference")
              (progn
                (setq blk (vlax-ename->vla-object ent))
                (setq blockname (vla-get-name blk))
                (setq image-file (strcat blockname ".png")) ; Generate image file name based on block name
                (setq insertion-point (vlax-curve-getstartpoint blk))
                (setq x (car insertion-point))
                (setq y (cadr insertion-point))
                (setq img (vla-copyobjects (list ent) mspace)) ; Copy block reference to modelspace to generate image
                (vla-put-layer img "0") ; Set image layer to "0" for visibility
                (vla-put-basepoint img (vlax-3d-point (list x y 0))) ; Set image basepoint
                (vla-put-rotation img (vla-get-rotation blk)) ; Set image rotation
                (vla-put-scalefactor img 1.0) ; Set image scale factor
                (vla-export img image-file) ; Export image as PNG
                (setq csv-row (list blockname image-file))
                (setq csv-content (append csv-content (list (apply 'strcat csv-row))))
              )
            )
          (setq i (1+ i))
          )
        (setq csv-file (vl-filename-mktemp (vl-filename-base csv-file) ".csv"))
        (vl-file-write (apply 'strcat (mapcar 'strcat csv-content '("\n"))) csv-file)
        (prompt (strcat "\nCSV file exported: " csv-file))
        )
    (prompt "\nهیچ بلاکی در سرتاسر نقشه درآن لایه که می‌خواهید یافت نشد.")
    )
   (vl-load-com)
    ; ask for file path
    (setq file_path (getfiled "Save CSV File" "" "csv" 1))

    ; create file
    (setq file (open file_path "w"))
    (setq csv-file (strcat "C:\\Path\\To\\Directory\\" csv-file)) ; Update the directory path accordingly
  
    (vl-file-write (apply 'strcat (mapcar 'strcat csv-content '("\n"))) csv-file)
    (prompt (strcat "\nCSV file exported: " csv-file))
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
      ;;(vla-put-Color layer acMagenta)
      (vla-put-Color layer acMagenta))) layer)
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================

(defun C:elu ()
  (create-layer "steels")
  (create-layer "blocks1285785")
  (setq j 1)
  (setq q0 (ssget "ALL" '((8 . "steels") (0 . "text"))))

  (setq opp "1")
                
  (while ( > j 0) 
    (put-text)
  ;; Prompt the user if they want to repeat
  (setq repeat-prompt (getstring t "\nDo you want to enter another length? [Y] or exit [e]"))
    (if(or (equal repeat-prompt "E") (equal repeat-prompt "e"))
       (progn
     
      (export-to-excel)
      (setq j 0))
    )
      )    
  )
;;=======================================================================================================================
;;=======================================================================================================================
;;=======================================================================================================================
