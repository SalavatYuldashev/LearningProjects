using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ULearnMe
{
    internal class ReverseNumber
    {
        public int ReverseOfNumber(int number)
        {
            string strNumber = number.ToString();
            string revNumber = null;

            for (int i = 0; i < strNumber.Length; i++)
            {
                revNumber += new String(strNumber.Substring(strNumber.Length-i-1,1));
            }
            
            return int.Parse(revNumber);
        }
    }
}
