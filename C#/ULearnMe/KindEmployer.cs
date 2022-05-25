using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ULearnMe
{
    internal class KindEmployer
    {
        public static string GetGreetingMessage(string name, double salary)
        {
            salary = Math.Ceiling(salary);
            string s = $"Hello, {name}, your salary is {salary}";
            return s;
        }
    }
}
