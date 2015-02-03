;;; leet-code.el --- leet-code client for emacs

;;; Commentary:
;; 
;; login, submit, check result, retrieve quest info

;;; Code:

(eval-when-compile (require 'cl))

(require 'request)
(require 'json )

(defconst leet-code-base-url "https://oj.leetcode.com")
(defconst leet-code-action-login "accounts/login/")
(defconst leet-code-action-logout "accounts/logout/")
(defconst leet-code-request-header
  '(("Referer" . "https://oj.leetcode.com/")
    ("Host" . "oj.leetcode.com")
    ("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:35.0) Gecko/20100101 Firefox/35.0")))
(defconst leet-code-csrf-key "csrfmiddlewaretoken")
(defconst leet-code-csrf-cookie-key "csrftoken")
(defvar *leet-code-last-submission-id* nil)

;;; url-retrieve is good at dealing with cookie
(setq request-backend 'url-retrieve)


(defun leet-code--file-to-string (file)
  "Read the content of FILE and return it as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defconst leet-code-quests '(("Reverse Words in a String II" 186)
                             ("Largest Number" 179)
                             ("Dungeon Game" 174)
                             ("Binary Search Tree Iterator" 173)
                             ("Factorial Trailing Zeroes" 172)
                             ("Excel Sheet Column Number" 171)
                             ("Two Sum III - Data structure design" 170)
                             ("Majority Element" 169)
                             ("Excel Sheet Column Title" 168)
                             ("Two Sum II - Input array is sorted" 167)
                             ("Fraction to Recurring Decimal" 166)
                             ("Compare Version Numbers" 165)
                             ("Maximum Gap" 164)
                             ("Missing Ranges" 163)
                             ("Find Peak Element" 162)
                             ("One Edit Distance" 161)
                             ("Intersection of Two Linked Lists" 160)
                             ("Longest Substring with At Most Two Distinct Characters" 159)
                             ("Read N Characters Given Read4 II - Call multiple times" 158)
                             ("Read N Characters Given Read4" 157)
                             ("Binary Tree Upside Down" 156)
                             ("Min Stack" 155)
                             ("Find Minimum in Rotated Sorted Array II" 154)
                             ("Find Minimum in Rotated Sorted Array" 153)
                             ("Maximum Product Subarray" 152)
                             ("Reverse Words in a String" 151)
                             ("Evaluate Reverse Polish Notation" 150)
                             ("Max Points on a Line" 149)
                             ("Sort List" 148)
                             ("Insertion Sort List" 147)
                             ("LRU Cache" 146)
                             ("Binary Tree Postorder Traversal" 145)
                             ("Binary Tree Preorder Traversal" 144)
                             ("Reorder List" 143)
                             ("Linked List Cycle II" 142)
                             ("Linked List Cycle" 141)
                             ("Word Break II" 140)
                             ("Word Break" 139)
                             ("Copy List with Random Pointer" 138)
                             ("Single Number II" 137)
                             ("Single Number" 136)
                             ("Candy" 135)
                             ("Gas Station" 134)
                             ("Clone Graph" 133)
                             ("Palindrome Partitioning II" 132)
                             ("Palindrome Partitioning" 131)
                             ("Surrounded Regions" 130)
                             ("Sum Root to Leaf Numbers" 129)
                             ("Longest Consecutive Sequence" 128)
                             ("Word Ladder" 127)
                             ("Word Ladder II" 126)
                             ("Valid Palindrome" 125)
                             ("Binary Tree Maximum Path Sum" 124)
                             ("Best Time to Buy and Sell Stock III" 123)
                             ("Best Time to Buy and Sell Stock II" 122)
                             ("Best Time to Buy and Sell Stock" 121)
                             ("Triangle" 120)
                             ("Pascal's Triangle II" 119)
                             ("Pascal's Triangle" 118)
                             ("Populating Next Right Pointers in Each Node II" 117)
                             ("Populating Next Right Pointers in Each Node" 116)
                             ("Distinct Subsequences" 115)
                             ("Flatten Binary Tree to Linked List" 114)
                             ("Path Sum II" 113)
                             ("Path Sum" 112)
                             ("Minimum Depth of Binary Tree" 111)
                             ("Balanced Binary Tree" 110)
                             ("Convert Sorted List to Binary Search Tree" 109)
                             ("Convert Sorted Array to Binary Search Tree" 108)
                             ("Binary Tree Level Order Traversal II" 107)
                             ("Construct Binary Tree from Inorder and Postorder Traversal" 106)
                             ("Construct Binary Tree from Preorder and Inorder Traversal" 105)
                             ("Maximum Depth of Binary Tree" 104)
                             ("Binary Tree Zigzag Level Order Traversal" 103)
                             ("Binary Tree Level Order Traversal" 102)
                             ("Symmetric Tree" 101)
                             ("Same Tree" 100)
                             ("Recover Binary Search Tree" 99)
                             ("Validate Binary Search Tree" 98)
                             ("Interleaving String" 97)
                             ("Unique Binary Search Trees" 96)
                             ("Unique Binary Search Trees II" 95)
                             ("Binary Tree Inorder Traversal" 94)
                             ("Restore IP Addresses" 93)
                             ("Reverse Linked List II" 92)
                             ("Decode Ways" 91)
                             ("Subsets II" 90)
                             ("Gray Code" 89)
                             ("Merge Sorted Array" 88)
                             ("Scramble String" 87)
                             ("Partition List" 86)
                             ("Maximal Rectangle" 85)
                             ("Largest Rectangle in Histogram" 84)
                             ("Remove Duplicates from Sorted List" 83)
                             ("Remove Duplicates from Sorted List II" 82)
                             ("Search in Rotated Sorted Array II" 81)
                             ("Remove Duplicates from Sorted Array II" 80)
                             ("Word Search" 79)
                             ("Subsets" 78)
                             ("Combinations" 77)
                             ("Minimum Window Substring" 76)
                             ("Sort Colors" 75)
                             ("Search a 2D Matrix" 74)
                             ("Set Matrix Zeroes" 73)
                             ("Edit Distance" 72)
                             ("Simplify Path" 71)
                             ("Climbing Stairs" 70)
                             ("Sqrt(x)" 69)
                             ("Text Justification" 68)
                             ("Add Binary" 67)
                             ("Plus One" 66)
                             ("Valid Number" 65)
                             ("Minimum Path Sum" 64)
                             ("Unique Paths II" 63)
                             ("Unique Paths" 62)
                             ("Rotate List" 61)
                             ("Permutation Sequence" 60)
                             ("Spiral Matrix II" 59)
                             ("Length of Last Word" 58)
                             ("Insert Interval" 57)
                             ("Merge Intervals" 56)
                             ("Jump Game" 55)
                             ("Spiral Matrix" 54)
                             ("Maximum Subarray" 53)
                             ("N-Queens II" 52)
                             ("N-Queens" 51)
                             ("Pow(x, n)" 50)
                             ("Anagrams" 49)
                             ("Rotate Image" 48)
                             ("Permutations II" 47)
                             ("Permutations" 46)
                             ("Jump Game II" 45)
                             ("Wildcard Matching" 44)
                             ("Multiply Strings" 43)
                             ("Trapping Rain Water" 42)
                             ("First Missing Positive" 41)
                             ("Combination Sum II" 40)
                             ("Combination Sum" 39)
                             ("Count and Say" 38)
                             ("Sudoku Solver" 37)
                             ("Valid Sudoku" 36)
                             ("Search Insert Position" 35)
                             ("Search for a Range" 34)
                             ("Search in Rotated Sorted Array" 33)
                             ("Longest Valid Parentheses" 32)
                             ("Next Permutation" 31)
                             ("Substring with Concatenation of All Words" 30)
                             ("Divide Two Integers" 29)
                             ("Implement strStr()" 28)
                             ("Remove Element" 27)
                             ("Remove Duplicates from Sorted Array" 26)
                             ("Reverse Nodes in k-Group" 25)
                             ("Swap Nodes in Pairs" 24)
                             ("Merge k Sorted Lists" 23)
                             ("Generate Parentheses" 22)
                             ("Merge Two Sorted Lists" 21)
                             ("Valid Parentheses" 20)
                             ("Remove Nth Node From End of List" 19)
                             ("4Sum" 18)
                             ("Letter Combinations of a Phone Number" 17)
                             ("3Sum Closest" 16)
                             ("3Sum" 15)
                             ("Longest Common Prefix" 14)
                             ("Roman to Integer" 13)
                             ("Integer to Roman" 12)
                             ("Container With Most Water" 11)
                             ("Regular Expression Matching" 10)
                             ("Palindrome Number" 9)
                             ("String to Integer (atoi)" 8)
                             ("Reverse Integer" 7)
                             ("ZigZag Conversion" 6)
                             ("Longest Palindromic Substring" 5)
                             ("Median of Two Sorted Arrays" 4)
                             ("Longest Substring Without Repeating Characters" 3)
                             ("Add Two Numbers" 2)
                             ("Two Sum" 1)
                             ))

(defconst leet-code-status-map
  '((10 "AC" )
   (11 "WA" )
   (12 "MLE" )
   (13 "OLE" )
   (14 "TLE" )
   (15 "RE" )
   (16 "IE" )
   (20 "CE" )
   (21 "UE" )))


(defun leet-code--fresh-request ()
  (if (eq request-backend 'url-retrieve)
      (url-cookie-clean-up)
    (when (file-exists-p (request--curl-cookie-jar))
      (delete-file (request--curl-cookie-jar))))

  (request leet-code-base-url
           :sync t
           :headers leet-code-request-header
           ))

(defun leet-code--login (uname pwd)
  (interactive
   (list
    (read-string "input username or email ")
    (read-passwd "intpu password ")
    )
   )
  (unless (leet-code--is-loggedin)
    (leet-code--fresh-request)
    (request (concat leet-code-base-url "/" leet-code-action-login)
             :type "POST"
             :data `(("login" . ,uname )
                     ("password" . ,pwd)
                     ("Host" . "oj.leetcode.com")
                     (,leet-code-csrf-key .
                                          ,(cdr
                                            (assoc
                                             leet-code-csrf-cookie-key
                                             (request-cookie-alist
                                              "oj.leetcode.com" "/" t)))))
             :headers leet-code-request-header)))

(defun leet-code--is-loggedin ()
  (string-match "Successfully signed in"
                (or (cdr
                     (assoc
                      "messages"
                      (request-cookie-alist "oj.leetcode.com" "/" t))) "")))


(defun leet-code--logout ()
  (interactive)
  (request (concat leet-code-base-url "/" leet-code-action-logout)
           :sync t
           :headers leet-code-request-header))


(defun* leet-code--submit
    (content &optional qname qid &key (type "large") (lang "cpp"))  
  (assert (leet-code--is-loggedin) t "login first")
  (unless (and qname qid)
    (let ((quest
           (assoc
            (completing-read "choose quest: " leet-code-quests nil t) 
            leet-code-quests
            )))
      (if quest
          (setq qname (car quest)  qid (cadr quest))
          (error "invalid quest"))
      )
    )
   
   (request
    (concat
     leet-code-base-url
     "/problems/" qname "/submit/" )
    :type "POST"
    :data `((,leet-code-csrf-key .
                                 ,(cdr (assoc leet-code-csrf-cookie-key
                                              (request-cookie-alist "oj.leetcode.com" "/" t)) )
                                 )
            ("lang" . ,lang)
            ("data_input" . "")
            ("question_id" . ,(number-to-string qid))
            ("judge_type" . ,type);large, small ?
            ("typed_code" . ,content)
            )
    :headers leet-code-request-header
    :parser 'json-read
    :success
    (function* (lambda (&key data &allow-other-keys)
                 (setq *leet-code-last-submission-id*
                       (number-to-string
                        (cdr (assoc 'submission_id data))))
                 (leet-code--check-submission)))))


(defun leet-code--submit-region (start end)
  (interactive "r")
  (leet-code--submit (buffer-substring start end)))


(defun leet-code--submit-file (file)
  (interactive (list
                (read-file-name
                 "select file: "
                 default-directory
                 (file-name-nondirectory buffer-file-name))))
  (leet-code--submit (leet-code--file-to-string file)))



(defun leet-code--check-submission (&optional sid)
  (assert (leet-code--is-loggedin) t "login first")
  (unless sid
    (setq sid *leet-code-last-submission-id*))
  (request
   (concat leet-code-base-url "/submissions/detail/" sid "/check/")
   :parser 'json-read
   :headers leet-code-request-header
   :success
   (function* (lambda (&key data &allow-other-keys)
                (let ((status-code (cdr (assoc 'status_code data)))
                      (state (cdr (assoc 'state data)))
                      )
                  (if (not (string= state "SUCCESS"))
                      (run-with-idle-timer
                       3
                       nil
                       'leet-code--check-submission)
                      (message (cadr
                            (assoc
                             status-code
                             leet-code-status-map)))))))))

(defun leet-code--parse-code (raw-content)
  (let ((re
         ;; I don't want to do this... but they don't provide me some API
         "<div\s*?class=\"col-md-12\"\s*?ng-init=\"init(\\(.*\\),\s*?'\\w+');\">"
         )
        code-str
        code-obj)
    (string-match re raw-content)
    (setq code-str (match-string-no-properties 1 raw-content))
    (with-temp-buffer
      (insert code-str)
      (while (search-backward "'" (point-min) t)
        (replace-match "\""))
      (goto-char 0)
      (setq code-obj (json-read))
      )
    (when (not (stringp code-obj))      ;no error thrown and result isn't str.
      (let ((cpp-entry                  ;why hard code? java devs use emacs are
                                        ;rarely, if you want other lang, send me
                                        ;PR or raise a issue. 
             (car (delq nil
                        (mapcar (lambda (entry)
                                  (and
                                   (string= (cdr (assoc 'value entry)) "cpp" )
                                   entry)) code-obj)))))
        (cdr
         (assoc 'defaultCode cpp-entry))))))

(defun leet-code--parse-info (raw-content)
  (substring-no-properties
   raw-content
   (string-match "<div\s?class=\"question-content\">" raw-content) ;malformatted
                                                                   ;html? it's
                                                                   ;ok to libxml
   (string-match "<div\s?id=\"tags\"\s?" raw-content)))

(defun leet-code--display-info (qname)
  (interactive
   (list
    (completing-read
     "choose question: "
     leet-code-quests
     nil
     t)))
  (let* ((data
          (request-response-data
           (request
            (concat leet-code-base-url "/problems/" qname "/")
            :parser 'buffer-string
            :headers leet-code-request-header
            :sync t
            )
           ))
         (info (leet-code--parse-info data))
         (start (point-min))
         (end (length info))
        )
    (save-excursion
      (goto-char start)
      (insert info)
      (newline)
      (shr-render-region start end)
      (comment-region start (point))
;      (fill-region start (point))
      (when (y-or-n-p "insert code template? ")
        (insert (leet-code--parse-code data))
        (newline)
          ))
    ))


(provide 'leet-code)

;;; leet-code.el ends here
