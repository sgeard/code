#include <iostream>

using namespace std;

namespace template_add_m {

    template<typename T>
    T add(T a, T b) {
        return a + b;
    }
    
    template<typename T>
    void check_result(T, T) {}
    template<>
    void check_result(int r, int ref) {
        cout << "The result is " << r << " => ";
        cout << (r != ref ? "FAILED" : "passed") << endl;
    }
    template<>
    void check_result(double r, double ref) {
        cout << "The result is " << r << " => ";
        cout << ((abs(r-ref) > 1e-5) ? "FAILED" : "passed") << endl;
    }
    
    void test_template() {
        {
            double x, y, r;
            x = 5.1;
            y = 7.2;
            r = add(x,y);
            check_result(r, 12.3);
        }
        {
            int a, b, r;
            a = 5;
            b = 9;
            r = add(a, b);
            check_result(r, 14);
        }
    }
}

int main(int, char**) {
    template_add_m::test_template();
    return 0;
}
    
