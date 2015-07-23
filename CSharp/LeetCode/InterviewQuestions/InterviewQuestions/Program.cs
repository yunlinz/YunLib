using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using InterviewQuestions.LeetCode;

namespace InterviewQuestions
{
    class Program
    {
        static void Main(string[] args)
        {
            foreach (int x in (new LeetCode.LeetCode()).ProductWithoutSelf(new int[] { 1, 2, 3, 4, 5, 6, 7 })) { Console.WriteLine(x); }
        }
    }
}
