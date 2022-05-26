using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ULearnMe.Errors
{
    internal class MinimumFunction
    {
        public static string GetMinX(int a, int b, int c)
        {
            if ((a > 0) || (a == 0 && b == 0 && (c == 2 || c == 0)))
            {
                return (-(b / (2.0 * a))).ToString();
            }
            return "Impossible";
        }
        public static void Invoke()
        {
            Console.WriteLine(GetMinX(1, 2, 3));
            Console.WriteLine(GetMinX(0, 3, 2));
            Console.WriteLine(GetMinX(1, -2, -3));
            Console.WriteLine(GetMinX(5, 2, 1));
            Console.WriteLine(GetMinX(4, 3, 2));
            Console.WriteLine(GetMinX(0, 4, 5));

            // А в этих случаях решение существует:
            Console.WriteLine(GetMinX(0, 0, 2) != "Impossible");
            Console.WriteLine(GetMinX(0, 0, 0) != "Impossible");
        }
    }
}
