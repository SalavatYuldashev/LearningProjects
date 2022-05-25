#include <algorithm>
#include <cmath>
#include <iostream>
#include <limits>
#include <numeric>
#include <vector>

char uselessGet(char c)
{
  std::vector<char> v(std::numeric_limits<char>::max());
  std::iota(v.begin(), v.end(), 0);

  auto ch = std::find_if(v.begin(), v.end(), [=](auto element){ return element == c; });
  
  return *ch;
}

int main()
{
  std::vector<bool> vv(200, false);

  for (size_t i = 0; i < 501; i++)
  {
    std::for_each(vv.begin(), vv.end(),
        [=](auto elem)
        {
          std::vector<char> v(std::numeric_limits<char>::max());
          std::iota(v.begin(), v.end(), 0);

          std::vector<char> v2;
          std::for_each(v.begin(), v.end(), [&](auto element){ v2.push_back(uselessGet(element)); });

          elem = i % 2 == 0 ? v == v2 : v != v2;
        });
  }

  for (size_t i = 0; i < 1000000; i++)
  {
    for (size_t i = 0; i < 600; i++)
    {
      int a = vv[i % 200] ? pow(i, i * i) : pow(i * i, i);
      (void)a;
    }
  }

  return 0;
}
