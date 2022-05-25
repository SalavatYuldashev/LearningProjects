# Лабораторная работа №4 «Выбор оптимально опции оптимизации»

### Таблица оптимизации

|   -O0   |   -Os   |   -O1   |   -O2   |   -O3   |
|---------|---------|---------|---------|---------|
| 61,523s | 20,430s | 15,481s | 16,572s | 14,375s |

| -O2 <br> -march=native | -O3 <br> -march=native | -O2 <br> -march=native <br> -funroll-loops | -O3 <br> -march=native <br> -funroll-loops |
|:----------------------:|:----------------------:|:------------------------------------------:|:------------------------------------------:|
|         14,169s        |         14,046s        |                  13,624s                   |                   12,292s                  |

#### Оптимальная версия
```bash
-O3 -march=native -funroll-loops
```

#### С межпроцедурной оптимизацией и оптимизацией времени компоновки
```bash
g++ -Wall -march=native -O3 -funroll-loops -flto -fipa-profile opt-this.cpp -o prg.veg
time ./prg.veg

./prg.veg 13,37s
```

__Итог - 13,37s__

#### С оптимизацией с обратной связью
```bash
g++ -Wall -march=native -O3 -funroll-loops -fprofile-generate opt-this.cpp -o prg.veg
time ./prg.veg

./prg.veg 13,02s
```

__Итог _fprofile-generate_ - 13,02s__

```bash
g++ -Wall -march=native -O3 -funroll-loops -fprofile-use opt-this.cpp -o prg.veg
time ./prg.veg

./prg.veg 13,88s
```

__Итог _fprofile-use_ - 13,88s__

#### MAXIMUM POWER ACTIVATED
```bash
g++ -Wall -march=native -O3 -funroll-loops -flto -fipa-profile -fprofile-generate opt-this.cpp -o prg.veg
time ./prg.veg

./prg.veg 13,19s
```

__Итог _fprofile-generate_ - 13,19s__

```bash
g++ -Wall -march=native -O3 -funroll-loops -flto -fipa-profile -fprofile-use opt-this.cpp -o prg.veg
time ./prg.veg

./prg.veg 12,56s
```

__Итог _fprofile-use_ - 12,56s__
