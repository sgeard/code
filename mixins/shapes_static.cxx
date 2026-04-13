#include <cmath>

// Base class has a pure virtual function for cloning
class AbstractShape {
public:
    virtual ~AbstractShape () = default;
    virtual double getArea() const = 0;
};

// This CRTP class implements clone() for Derived
template <typename Derived>
class Shape : public AbstractShape {
public:
    double getArea() const override {
        return getArea<Derived>();
    };
    /*
    std::unique_ptr<AbstractShape> clone() const override {
        return std::make_unique<Derived>(static_cast<Derived const&>(*this));
    }
    */
    
private:
    
protected:
   // We make clear Shape class needs to be inherited
   Shape() = default;
   Shape(const Shape&) = default;
   Shape(Shape&&) = default;
   Shape(double a, double b) : a(a), b(b) {};
    double a = 0;
    double b = 0;
};
// Every derived class inherits from CRTP class instead of abstract class

class Square : public Shape<Square> {
public:
    Square(double a, double b) : a(a), b(b) {};
    double getArea() const {return a*b;}
private:
    double a = 0;
    double b = 0;
};

class Circle : public Shape<Circle> {
    double getArea() const {return 4*atan(1.0)*a*b;}
};


#include <iostream>
using namespace std;

int main(int, char**) {
    Square s(5.0,5.0);
}
