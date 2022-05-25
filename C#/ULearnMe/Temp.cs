using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ULearnMe
{
    internal class Temp
    {
		static string who = "class";

		 public static void F()
		{
			string who = "F";
		}

		public static void G()
		{
			F();
			Console.WriteLine(who);
		}

		public static void H()
		{
			string who = "H";
			F();
			Console.Write(who);
		}

	}
}
