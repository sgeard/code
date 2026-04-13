#include "auto_d.hxx"
#include <iostream>
#include <sstream>
#include <functional>

using namespace std;

template<int N>
bool check_result(const ad_var<N>& r, const ad_var<N>& ref) {
    if (r == ref)
        return true;
    
    cout << endl << "result = " << r.as_string() << " != reference = " << ref.as_string() << endl;
    //throw runtime_error("FAILED");
    return false;
};


int main(int argc, char** argv)
{
    using D2 = ad_var<2>;
    using D3 = ad_var<3>;
    using D4 = ad_var<4>;
   
    auto do_2nd = [] () {
        cout << "2nd derivatives:" << endl;
        D2 s(0.5);
        D2 u(sin(s));
        D2 v(cos(s));
        D2 w = u*v; // w = sin(x)*cos(x) == 0.5*sin(2x) :: D(w) = cos(2x) ; DD(w) = -2*sin(2x)
        D2 ref({0.5*sin(1),cos(1),-2*sin(1)});
        cout << "\tsin(x)*cos(x) ... ";
        if (check_result(w,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
        cout << "\t0.5*sin(2x) ... ";
        D2 z = 0.5*sin(2*D2(0.5));
        if (check_result(z,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
        cout << "\t0.5*cos(2x) ... ";
        z = 0.5*cos(2*D2(0.5));
        ref = {0.5*cos(1),-sin(1),-2*cos(1)};
        if (check_result(z,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
        cout << "\tx**3 + x ... ";
        D2 p = pow(s,3) + s;
        ref = {pow(0.5,3)+0.5,3*pow(0.5,2) + 1,6*0.5};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
        cout << "\tx**5 ... ";
        p = pow(s,5);
        ref = {pow(0.5,5),5*pow(0.5,4),20*pow(0.5,3)};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
        cout << "\t(2-3x)**5 ... ";
        double t = 2-3*0.5;
        D2 q = pow(2-3*s,5);
        ref = {pow(t,5),5*(-3)*pow(t,4),5*4*(-3)*(-3)*pow(t,3)};
        if (check_result(q,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
        cout << "\t(x + 2)*(x + 3)*(x - 1) ... ";
        q = (s+2)*(s+3)*(s-1);
        ref = {(0.5+2)*(0.5+3)*(0.5-1),3*pow(0.5,2)+8*0.5+1,6*0.5+8};
        if (check_result(q,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
       
        cout << "\t1/x ... ";
        p = 1/s;
        ref = {pow(0.5,-1),-pow(0.5,-2),2*pow(0.5,-3)};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }

        cout << "\t1/sqrt(4-x**2) ... ";
        p = 1/sqrt(4-pow(s,2));
        double t3 = 1/sqrt(4-pow(0.5,2));
        ref = {t3,0.5*pow(t3,3),pow(t3,3)+3*pow(0.5,2)*pow(t3,5)};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }

        cout << "\te**(3x) ... ";
        p = exp(3*s);
        double t4 = exp(3*0.5);
        ref = {t4,3*t4,9*t4};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }

        cout << "\t2**(2x) ... ";
        double l2 = log(2);
        p = exp2(2*s);
        double t5 = exp2(1);
        ref = {t5,2*l2*t5,4*l2*l2*t5};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }

        
        cout << "\tlog(3x) ... ";
        p = log(3*s);
        ref = {log(1.5),1/0.5,-1/pow(0.5,2)};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
    };

    auto do_3rd = [] () {

        cout << "3rd derivatives:" << endl;
        D3 s(0.5);
        D3 u(sin(s));
        D3 v(cos(s));
        D3 w = u*v; // w = sin(x)*cos(x) == 0.5*sin(2x) :: D(w) = cos(2x) ; DD(w) = -2*sin(2x)
        {
        D3 ref({0.5*sin(1),cos(1),-2*sin(1),-4*cos(1)});
        cout << "\tsin(x)*cos(x) ... ";
        if (check_result(w,ref)) {
            cout << "passed" << endl;
            //cout << w.as_string() << endl;
        } else {
            cout << "FAILED" << endl;
        }
        }
        
        {
        cout << "\t0.5*sin(2x) ... ";
        D3 z = 0.5*sin(2*D3(0.5));
        D3 ref({0.5*sin(1),cos(1),-2*sin(1),-4*cos(1)});
        if (check_result(z,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        }
        
        cout << "\t0.5*cos(2x) ... ";
        D3 z = 0.5*cos(2*D3(0.5));
        D3 ref = {0.5*cos(1),-sin(1),-2*cos(1),4*sin(1)};
        if (check_result(z,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
        cout << "\tx**3 + x ... ";
        D3 p = pow(s,3) + s;
        ref = {pow(0.5,3)+0.5,3*pow(0.5,2) + 1,6*0.5, 6};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
        cout << "\tx**5 ... ";
        p = pow(s,5);
        ref = {pow(0.5,5),5*pow(0.5,4),20*pow(0.5,3),60*pow(0.5,2)};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
        cout << "\t(2-3x)**5 ... ";
        double t = 2-3*0.5;
        D3 q = pow(2-3*s,5);
        ref = {pow(t,5),5*(-3)*pow(t,4),5*4*(-3)*(-3)*pow(t,3),5*4*3*(-3)*(-3)*(-3)*pow(t,2)};
        if (check_result(q,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
        cout << "\t(x + 2)*(x + 3)*(x - 1) ... ";
        q = (s+2)*(s+3)*(s-1);
        ref = {(0.5+2)*(0.5+3)*(0.5-1),3*pow(0.5,2)+8*0.5+1,6*0.5+8,6};
        if (check_result(q,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
       
        cout << "\t1/x ... ";
        p = 1/s;
        ref = {pow(0.5,-1),-pow(0.5,-2),2*pow(0.5,-3),-6*pow(0.5,-4)};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }

        cout << "\t1/sqrt(4-x**2) ... ";
        p = 1/sqrt(4-pow(s,2));
        double t3 = 1/sqrt(4-pow(0.5,2));
        ref = {
            t3,
            0.5*pow(t3,3),
            pow(t3,3)+3*pow(0.5,2)*pow(t3,5),
            3*0.5*pow(t3,5)*(3+5*pow(0.5*t3,2))
            };
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }

        cout << "\te**(3x) ... ";
        p = exp(3*s);
        double t4 = exp(3*0.5);
        ref = {t4,3*t4,9*t4,27*t4};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }

        cout << "\t2**(2x) ... ";
        double l2 = log(2);
        p = exp2(2*s);
        double t5 = exp2(1);
        ref = {t5,2*l2*t5,4*l2*l2*t5,8*l2*l2*l2*t5};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }

        cout << "\tD3[log(3x)] ... ";
        p = log(3*s);
        ref = {log(1.5),1/0.5,-1/pow(0.5,2),2/pow(0.5,3)};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }

        cout << "\tD3[pow(1+pow(s,7),-0.5)] ... ";
        p = pow(1+pow(s,7),-0.5);
        cout << p.as_string() << endl;
    };

    auto do_4th = [] () {
         cout << "4th derivatives:" << endl;
         D4 s(0.5);
         D4 u(sin(s));
         D4 v(cos(s));
         D4 w = u*v; // w = sin(x)*cos(x) == 0.5*sin(2x) :: D(w) = cos(2x) ; DD(w) = -2*sin(2x)
         D4 ref({0.5*sin(1),cos(1),-2*sin(1),-4*cos(1),8*sin(1)});
         cout << "\tsin(x)*cos(x) ... ";
         if (check_result(w,ref)) {
             cout << "passed" << endl;
         } else {
             cout << "FAILED" << endl;
         }
        
        cout << "\t0.5*sin(2x) ... ";
        D4 z = 0.5*sin(2*D4(0.5));
        if (check_result(z,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        //return;
        cout << "\t0.5*cos(2x) ... ";
        z = 0.5*cos(2*D4(0.5));
        ref = {0.5*cos(1),-sin(1),-2*cos(1),4*sin(1),8*cos(1)};
        if (check_result(z,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }

        cout << "\tx**3 + x ... ";
        D4 p = pow(s,3) + s;
        ref = {pow(0.5,3)+0.5,3*pow(0.5,2) + 1,6*0.5, 6, 0};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
        cout << "\tx**5 ... ";
        p = pow(s,5);
        ref = {pow(0.5,5),5*pow(0.5,4),20*pow(0.5,3),60*pow(0.5,2),120*0.5};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
        cout << "\t(2-3x)**5 ... ";
        double t = 2-3*0.5;
        D4 q = pow(2-3*s,5);
        ref = {pow(t,5),5*(-3)*pow(t,4),5*4*(-3)*(-3)*pow(t,3),5*4*3*(-3)*(-3)*(-3)*pow(t,2),5*4*3*2*(-3)*(-3)*(-3)*(-3)*t};
        if (check_result(q,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
        cout << "\t(x + 2)*(x + 3)*(x - 1) ... ";
        q = (s+2)*(s+3)*(s-1);
        ref = {(0.5+2)*(0.5+3)*(0.5-1),3*pow(0.5,2)+8*0.5+1,6*0.5+8,6,0};
        if (check_result(q,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
       
        cout << "\t1/x ... ";
        p = 1/s;
        ref = {pow(0.5,-1),-pow(0.5,-2),2*pow(0.5,-3),-6*pow(0.5,-4),24*pow(0.5,-5)};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }

        cout << "\t1/sqrt(4-x**2) ... ";
        p = 1/sqrt(4-pow(s,2));
        double t3 = 1/sqrt(4-pow(0.5,2));
        double r4 = 24*(pow(0.5,4)+12*pow(0.5,2)+6)*pow(t3,9);
        ref = {
            t3,
            0.5*pow(t3,3),
            pow(t3,3)+3*pow(0.5,2)*pow(t3,5),
            3*0.5*pow(t3,5)*(3+5*pow(0.5*t3,2)),
            r4
            };
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }

        cout << "\te**(3x) ... ";
        p = exp(3*s);
        double t4 = exp(3*0.5);
        ref = {t4,3*t4,9*t4,27*t4,81*t4};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }

        cout << "\t2**(2x) ... ";
        double l2 = log(2);
        p = exp2(2*s);
        double t5 = exp2(1);
        ref = {t5,2*l2*t5,4*l2*l2*t5,8*l2*l2*l2*t5,16*l2*l2*l2*l2*t5};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }

        
        cout << "\tlog(3x) ... ";
        p = log(3*s);
        ref = {log(1.5),1/0.5,-1/pow(0.5,2),2/pow(0.5,3),-6/pow(0.5,4)};
        if (check_result(p,ref)) {
            cout << "passed" << endl;
        } else {
            cout << "FAILED" << endl;
        }
        
    };
    
    if (argc == 1) {
        do_2nd();
        do_3rd();
        do_4th();
        
    } else {
        for (int i=1; i < argc; ++i) {
            istringstream s(argv[i]);
            int a = 0;
            s >> a;
            if (a == 2)
                do_2nd();
            else if(a == 3)
                do_3rd();
            else if(a == 4)
                do_4th();
        }
    }

    return 0;
}