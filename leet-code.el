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

;;; evulate this in firefox's developer tool to get latest problem lists.
;;;$x("//table[@id='problemList']/tbody/tr/td[2]/text()|//td[3]/a/text()|//td[5]/text()").reverse().map(function(e, i){return ["(\"[{0}]", "{1}\"", "{2})\n"][i % 3].replace("{"+ i % 3 +"}", e.nodeValue);}).join(" ")
(defconst leet-code-quests '(
                             ("[Medium] Two Sum" 1)
                             ("[Medium] Add Two Numbers" 2)
                             ("[Medium] Longest Substring Without Repeating Characters" 3)
                             ("[Hard] Median of Two Sorted Arrays" 4)
                             ("[Medium] Longest Palindromic Substring" 5)
                             ("[Easy] ZigZag Conversion" 6)
                             ("[Easy] Reverse Integer" 7)
                             ("[Easy] String to Integer (atoi)" 8)
                             ("[Easy] Palindrome Number" 9)
                             ("[Hard] Regular Expression Matching" 10)
                             ("[Medium] Container With Most Water" 11)
                             ("[Medium] Integer to Roman" 12)
                             ("[Easy] Roman to Integer" 13)
                             ("[Easy] Longest Common Prefix" 14)
                             ("[Medium] 3Sum" 15)
                             ("[Medium] 3Sum Closest" 16)
                             ("[Medium] Letter Combinations of a Phone Number" 17)
                             ("[Medium] 4Sum" 18)
                             ("[Easy] Remove Nth Node From End of List" 19)
                             ("[Easy] Valid Parentheses" 20)
                             ("[Easy] Merge Two Sorted Lists" 21)
                             ("[Medium] Generate Parentheses" 22)
                             ("[Hard] Merge k Sorted Lists" 23)
                             ("[Medium] Swap Nodes in Pairs" 24)
                             ("[Hard] Reverse Nodes in k-Group" 25)
                             ("[Easy] Remove Duplicates from Sorted Array" 26)
                             ("[Easy] Remove Element" 27)
                             ("[Easy] Implement strStr()" 28)
                             ("[Medium] Divide Two Integers" 29)
                             ("[Hard] Substring with Concatenation of All Words" 30)
                             ("[Medium] Next Permutation" 31)
                             ("[Hard] Longest Valid Parentheses" 32)
                             ("[Hard] Search in Rotated Sorted Array" 33)
                             ("[Medium] Search for a Range" 34)
                             ("[Medium] Search Insert Position" 35)
                             ("[Easy] Valid Sudoku" 36)
                             ("[Hard] Sudoku Solver" 37)
                             ("[Easy] Count and Say" 38)
                             ("[Medium] Combination Sum" 39)
                             ("[Medium] Combination Sum II" 40)
                             ("[Hard] First Missing Positive" 41)
                             ("[Hard] Trapping Rain Water" 42)
                             ("[Medium] Multiply Strings" 43)
                             ("[Hard] Wildcard Matching" 44)
                             ("[Hard] Jump Game II" 45)
                             ("[Medium] Permutations" 46)
                             ("[Hard] Permutations II" 47)
                             ("[Medium] Rotate Image" 48)
                             ("[Medium] Anagrams" 49)
                             ("[Medium] Pow(x, n)" 50)
                             ("[Hard] N-Queens" 51)
                             ("[Hard] N-Queens II" 52)
                             ("[Medium] Maximum Subarray" 53)
                             ("[Medium] Spiral Matrix" 54)
                             ("[Medium] Jump Game" 55)
                             ("[Hard] Merge Intervals" 56)
                             ("[Hard] Insert Interval" 57)
                             ("[Easy] Length of Last Word" 58)
                             ("[Medium] Spiral Matrix II" 59)
                             ("[Medium] Permutation Sequence" 60)
                             ("[Medium] Rotate List" 61)
                             ("[Medium] Unique Paths" 62)
                             ("[Medium] Unique Paths II" 63)
                             ("[Medium] Minimum Path Sum" 64)
                             ("[Hard] Valid Number" 65)
                             ("[Easy] Plus One" 66)
                             ("[Easy] Add Binary" 67)
                             ("[Hard] Text Justification" 68)
                             ("[Medium] Sqrt(x)" 69)
                             ("[Easy] Climbing Stairs" 70)
                             ("[Medium] Simplify Path" 71)
                             ("[Hard] Edit Distance" 72)
                             ("[Medium] Set Matrix Zeroes" 73)
                             ("[Medium] Search a 2D Matrix" 74)
                             ("[Medium] Sort Colors" 75)
                             ("[Hard] Minimum Window Substring" 76)
                             ("[Medium] Combinations" 77)
                             ("[Medium] Subsets" 78)
                             ("[Medium] Word Search" 79)
                             ("[Medium] Remove Duplicates from Sorted Array II" 80)
                             ("[Medium] Search in Rotated Sorted Array II" 81)
                             ("[Medium] Remove Duplicates from Sorted List II" 82)
                             ("[Easy] Remove Duplicates from Sorted List" 83)
                             ("[Hard] Largest Rectangle in Histogram" 84)
                             ("[Hard] Maximal Rectangle" 85)
                             ("[Medium] Partition List" 86)
                             ("[Hard] Scramble String" 87)
                             ("[Easy] Merge Sorted Array" 88)
                             ("[Medium] Gray Code" 89)
                             ("[Medium] Subsets II" 90)
                             ("[Medium] Decode Ways" 91)
                             ("[Medium] Reverse Linked List II" 92)
                             ("[Medium] Restore IP Addresses" 93)
                             ("[Medium] Binary Tree Inorder Traversal" 94)
                             ("[Medium] Unique Binary Search Trees II" 95)
                             ("[Medium] Unique Binary Search Trees" 96)
                             ("[Hard] Interleaving String" 97)
                             ("[Medium] Validate Binary Search Tree" 98)
                             ("[Hard] Recover Binary Search Tree" 99)
                             ("[Easy] Same Tree" 100)
                             ("[Easy] Symmetric Tree" 101)
                             ("[Easy] Binary Tree Level Order Traversal" 102)
                             ("[Medium] Binary Tree Zigzag Level Order Traversal" 103)
                             ("[Easy] Maximum Depth of Binary Tree" 104)
                             ("[Medium] Construct Binary Tree from Preorder and Inorder Traversal" 105)
                             ("[Medium] Construct Binary Tree from Inorder and Postorder Traversal" 106)
                             ("[Easy] Binary Tree Level Order Traversal II" 107)
                             ("[Medium] Convert Sorted Array to Binary Search Tree" 108)
                             ("[Medium] Convert Sorted List to Binary Search Tree" 109)
                             ("[Easy] Balanced Binary Tree" 110)
                             ("[Easy] Minimum Depth of Binary Tree" 111)
                             ("[Easy] Path Sum" 112)
                             ("[Medium] Path Sum II" 113)
                             ("[Medium] Flatten Binary Tree to Linked List" 114)
                             ("[Hard] Distinct Subsequences" 115)
                             ("[Medium] Populating Next Right Pointers in Each Node" 116)
                             ("[Hard] Populating Next Right Pointers in Each Node II" 117)
                             ("[Easy] Pascal's Triangle" 118)
                             ("[Easy] Pascal's Triangle II" 119)
                             ("[Medium] Triangle" 120)
                             ("[Medium] Best Time to Buy and Sell Stock" 121)
                             ("[Medium] Best Time to Buy and Sell Stock II" 122)
                             ("[Hard] Best Time to Buy and Sell Stock III" 123)
                             ("[Hard] Binary Tree Maximum Path Sum" 124)
                             ("[Easy] Valid Palindrome" 125)
                             ("[Hard] Word Ladder II" 126)
                             ("[Medium] Word Ladder" 127)
                             ("[Hard] Longest Consecutive Sequence" 128)
                             ("[Medium] Sum Root to Leaf Numbers" 129)
                             ("[Medium] Surrounded Regions" 130)
                             ("[Medium] Palindrome Partitioning" 131)
                             ("[Hard] Palindrome Partitioning II" 132)
                             ("[Medium] Clone Graph" 133)
                             ("[Medium] Gas Station" 134)
                             ("[Hard] Candy" 135)
                             ("[Medium] Single Number" 136)
                             ("[Medium] Single Number II" 137)
                             ("[Hard] Copy List with Random Pointer" 138)
                             ("[Medium] Word Break" 139)
                             ("[Hard] Word Break II" 140)
                             ("[Medium] Linked List Cycle" 141)
                             ("[Medium] Linked List Cycle II" 142)
                             ("[Medium] Reorder List" 143)
                             ("[Medium] Binary Tree Preorder Traversal" 144)
                             ("[Hard] Binary Tree Postorder Traversal" 145)
                             ("[Hard] LRU Cache" 146)
                             ("[Medium] Insertion Sort List" 147)
                             ("[Medium] Sort List" 148)
                             ("[Hard] Max Points on a Line" 149)
                             ("[Medium] Evaluate Reverse Polish Notation" 150)
                             ("[Medium] Reverse Words in a String" 151)
                             ("[Medium] Maximum Product Subarray" 152)
                             ("[Medium] Find Minimum in Rotated Sorted Array" 153)
                             ("[Hard] Find Minimum in Rotated Sorted Array II" 154)
                             ("[Easy] Min Stack" 155)
                             ("[Medium] Binary Tree Upside Down" 156)
                             ("[Easy] Read N Characters Given Read4" 157)
                             ("[Hard] Read N Characters Given Read4 II - Call multiple times" 158)
                             ("[Hard] Longest Substring with At Most Two Distinct Characters" 159)
                             ("[Easy] Intersection of Two Linked Lists" 160)
                             ("[Medium] One Edit Distance" 161)
                             ("[Medium] Find Peak Element" 162)
                             ("[Medium] Missing Ranges" 163)
                             ("[Hard] Maximum Gap" 164)
                             ("[Easy] Compare Version Numbers" 165)
                             ("[Medium] Fraction to Recurring Decimal" 166)
                             ("[Medium] Two Sum II - Input array is sorted" 167)
                             ("[Easy] Excel Sheet Column Title" 168)
                             ("[Easy] Majority Element" 169)
                             ("[Easy] Two Sum III - Data structure design" 170)
                             ("[Easy] Excel Sheet Column Number" 171)
                             ("[Easy] Factorial Trailing Zeroes" 172)
                             ("[Medium] Binary Search Tree Iterator" 173)
                             ("[Hard] Dungeon Game" 174)
                             ("[Medium] Largest Number" 179)
                             ("[Medium] Reverse Words in a String II" 186)
                             ("[Medium] Repeated DNA Sequences" 187)
                             ("[Hard] Best Time to Buy and Sell Stock IV" 188)
                             ("[Easy] Rotate Array" 189)
                             ("[Easy] Reverse Bits" 190)
                             ("[Easy] Number of 1 Bits" 191)
                             ("[Easy] House Robber" 198)
                             ("[Medium] Binary Tree Right Side View" 199)
                             ("[Medium] Number of Islands" 200)
                             ("[Medium] Bitwise AND of Numbers Range" 201)
                             ("[Easy] Happy Number" 202)
                             ("[Easy] Remove Linked List Elements" 203)
                             ("[Easy] Count Primes" 204)
                             ("[Easy] Isomorphic Strings" 205)
                             ("[Easy] Reverse Linked List" 206)
                             ("[Medium] Course Schedule" 207)
                             ("[Medium] Implement Trie (Prefix Tree)" 208)
                             ("[Medium] Minimum Size Subarray Sum" 209)
                             ("[Medium] Course Schedule II" 210)
                             ("[Medium] Add and Search Word - Data structure design" 211)
                             ("[Hard] Word Search II" 212)
                             ("[Medium] House Robber II" 213)
                             ("[Hard] Shortest Palindrome" 214)
                             ("[Medium] Kth Largest Element in an Array" 215)
                             ("[Medium] Combination Sum III" 216)
                             ("[Easy] Contains Duplicate" 217)
                             ("[Hard] The Skyline Problem" 218)
                             ("[Easy] Contains Duplicate II" 219)
                             ("[Medium] Contains Duplicate III" 220)
                             ("[Medium] Maximal Square" 221)
                             ("[Medium] Count Complete Tree Nodes" 222)
                             ("[Easy] Rectangle Area" 223)
                             ("[Medium] Basic Calculator" 224)
                             ("[Easy] Implement Stack using Queues" 225)
                             ("[Easy] Invert Binary Tree" 226)
                             ("[Medium] Basic Calculator II" 227)
                             ("[Easy] Summary Ranges" 228)
                             ("[Medium] Majority Element II" 229)
                             ("[Medium] Kth Smallest Element in a BST" 230)
                             ("[Easy] Power of Two" 231)
                             ("[Easy] Implement Queue using Stacks" 232)
                             ("[Medium] Number of Digit One" 233)
                             ("[Easy] Palindrome Linked List" 234)
                             ("[Easy] Lowest Common Ancestor of a Binary Search Tree" 235)
                             ("[Medium] Lowest Common Ancestor of a Binary Tree" 236)
                             ("[Easy] Delete Node in a Linked List" 237)
                             ("[Medium] Product of Array Except Self" 238)
                             ("[Hard] Sliding Window Maximum" 239)
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
