using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ULearnMe
{
    internal class WantedMethods
    {
        public static string GetLastHalf(string text)
        {

            string subString = text.Substring(text.Length / 2, text.Length / 2);
            return subString.Replace(" ", "");
        }
    }
}