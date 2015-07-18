using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using InterviewQuestions.Datatypes;

namespace InterviewQuestions.LeetCode
{
    class LeetCode
    {
        /// <summary>
        /// Given an array of integers, find two numbers such that they add up to a specific target number.
        /// The function twoSum should return indices of the two numbers such that they add up to the target, where index1 must be less than index2. Please note that your returned answers (both index1 and index2) are not zero-based.
        /// You may assume that each input would have exactly one solution.
        /// Input: numbers={2, 7, 11, 15}, target=9
        /// Output: index1=1, index2=2
        /// </summary>
        /// <param name="nums"></param>
        /// <param name="target"></param>
        /// <returns></returns>
        public int[] TwoSum(int[] nums, int target)
        {
            // nested for loop is too slow 
            // dictionary to store all values cannot handle duplicates case
            Dictionary<int, List<int>> numHash = new Dictionary<int, List<int>>();
            for (int i = 0; i < nums.Length; i++)
            {
                if (!numHash.ContainsKey(nums[i])) numHash.Add(nums[i], new List<int>());
                numHash[nums[i]].Add(i + 1);
            }
            List<int> idxList;
            int idx1;
            int[] indices = new int[2];
            foreach (KeyValuePair<int, List<int>> kv in numHash)
            {
                if (!numHash.TryGetValue(target - kv.Key, out idxList)) continue;
                idx1 = kv.Value.Min();
                foreach (int i in idxList) 
                {
                    if (i != idx1) 
                    {
                        indices[0] = Math.Min(i, idx1);
                        indices[1] = Math.Max(i, idx1);
                    }
                        
                }
            }
            return indices; 
        }


        /// <summary>
        /// You are given two linked lists representing two non-negative numbers. The digits are stored in reverse order and each of their nodes contain a single digit. Add the two numbers and return it as a linked list.
        /// Input: (2 -> 4 -> 3) + (5 -> 6 -> 4)
        /// Output: 7 -> 0 -> 8
        /// </summary>
        /// <param name="l1"></param>
        /// <param name="l2"></param>
        /// <returns></returns>
        public ListNode AddTwoNumbers(ListNode l1, ListNode l2)
        {
            ListNode head = new ListNode(0);
            ListNode last = head;
            ListNode curNode = head;
            bool carry = false;
            while (!(l1 == null && l2 == null))
            {
                l1 = l1 ?? (new ListNode(0));
                l2 = l2 ?? (new ListNode(0));

                curNode.val = l1.val + l2.val + (carry ? 1 : 0);
                curNode.next = new ListNode(0);
                if (curNode.val >= 10)
                {
                    curNode.val %= 10;
                    carry = true;
                }
                else carry = false;
                if (curNode != head) last = last.next; 
                curNode = curNode.next;
                l1 = l1.next;
                l2 = l2.next;
            }
            if (carry)
            {
                curNode.val = 1;
            }
            else last.next = null;
            return head;
        }


        /// <summary>
        /// Given a string, find the length of the longest substring without repeating characters. For example, the longest substring without repeating letters for "abcabcbb" is "abc", which the length is 3. For "bbbbb" the longest substring is "b", with the length of 1.
        /// </summary>
        /// <param name="s"></param>
        /// <returns></returns>
        public int LengthOfLongestSubstring(string s)
        {
            if (s == "") return 0;
            int idx2 = 0;
            int idx1 = 0;
            int longest = 1;
            HashSet<char> charHash = new HashSet<char>();
            charHash.Add(s[0]);
            int sLen = s.Length;

            while (idx2 < sLen - 1)
            {
                idx2++;
                if (charHash.Contains(s[idx2]))
                {
                    while (idx1 <= idx2 && s[idx1] != s[idx2]) 
                    {
                        charHash.Remove(s[idx1]);
                        idx1++;
                    }
                    idx1++;
                }
                else
                {
                    charHash.Add(s[idx2]);
                }
                longest = Math.Max(longest, charHash.Count);
            }
            return longest;
        }

        /// <summary>
        /// There are two sorted arrays nums1 and nums2 of size m and n respectively. Find the median of the two sorted arrays. The overall run time complexity should be O(log (m+n)).
        /// </summary>
        /// <param name="nums1"></param>
        /// <param name="nums2"></param>
        /// <returns></returns>
        public double FindMedianSortedArrays(int[] nums1, int[] nums2)
        {
            int n1Len = nums1.Length;
            int n2Len = nums2.Length;
            int[] longer = (n1Len > n2Len) ? nums1 : nums2;
            int[] shorter = (n1Len < n2Len) ? nums1 : nums2;
            int longLen = Math.Max(n1Len, n2Len);
            int shortLen = Math.Min(n1Len, n2Len);

            int idx1 = (int)Math.Floor(longLen / 2.0);
            int idx2 = 0;
            while (((idx1 + idx2)*2) != (longLen + shortLen))
            {

            }

            return (longer[idx1] + shorter[idx2]) / 2;

            // modified quickselect where there has to be 
        }



    }
}
