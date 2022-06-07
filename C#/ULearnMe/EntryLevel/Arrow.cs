using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ULearnMe.EntryLevel
{
    internal class Arrow
    {
        public int AngleBetweenArrows(int time)
        {
            int angle;
            if (time > 11)
            {
                time = time - 12;
            }
            if (time < 6)
            {
                angle = time * 30;
            }
            else
            {
                angle = 180 - ((time - 6) * 30);
            }
                        return angle;
        }
    }
}
