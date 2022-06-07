using System.ComponentModel.DataAnnotations;

namespace Automarket.Domain.Enum
{
    public enum TypeCar
    {
    [Display(Name = "Седан")]
    Sedan = 0,
    [Display(Name = "Внедорожник")]
    Suv = 1,
    [Display(Name = "Минивен")]
    Minivan = 2,
    [Display(Name = "Кабриолет")]
    Cabriolet = 3,
    [Display(Name = "Пикап")]
    PickupTruck = 4,
    [Display(Name = "Лимузин")]
    Limousine = 5
    }
}