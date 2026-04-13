// Written by Simon Geard, January 2020
//
// This header file implements automatic differention in a way
// that is naturally extensible to higher orders. It currently
// implements up to order 4
//
#ifndef auto_d_included
#define auto_d_included

#include <array>
#include <sstream>
#include <cmath>
#include <iostream>
#include <initializer_list>
#include <map>

enum class f_t {
    sin_f,
    cos_f,
    sinh_f,
    cosh_f,
    asin_f,
    acos_f,
    atan_f,
    exp_f,
    log_f
};

inline double D0_sin(double x) {return std::sin(x);}
inline double D1_sin(double x) {return std::cos(x);}
inline double D2_sin(double x) {return -std::sin(x);}
inline double D3_sin(double x) {return -std::cos(x);}
inline double D4_sin(double x) {return std::sin(x);}

inline double D0_cos(double x) {return std::cos(x);}
inline double D1_cos(double x) {return -std::sin(x);}
inline double D2_cos(double x) {return -std::cos(x);}
inline double D3_cos(double x) {return std::sin(x);}
inline double D4_cos(double x) {return std::cos(x);}

inline double D0_sinh(double x) {return std::sinh(x);}
inline double D1_sinh(double x) {return std::cosh(x);}
inline double D2_sinh(double x) {return std::sinh(x);}
inline double D3_sinh(double x) {return std::cosh(x);}
inline double D4_sinh(double x) {return std::sinh(x);}

inline double D0_cosh(double x) {return std::cosh(x);}
inline double D1_cosh(double x) {return std::sinh(x);}
inline double D2_cosh(double x) {return std::cosh(x);}
inline double D3_cosh(double x) {return std::sinh(x);}
inline double D4_cosh(double x) {return std::cosh(x);}

inline double D0_asin(double x) {return std::asin(x);}
inline double D1_asin(double x) {return std::pow(1-x*x,-0.5);}
inline double D2_asin(double x) {return x*pow(1-x*x,-1.5);}
inline double D3_asin(double x) {return (1+2*x*x)*pow(1-x*x,-2.5);}
inline double D4_asin(double x) {return 3*x*(2*x+3)*pow(1-x*x,-3.5);}

inline double D0_acos(double x) {return std::acos(x);}
inline double D1_acos(double x) {return -D1_asin(x);}
inline double D2_acos(double x) {return -D2_asin(x);}
inline double D3_acos(double x) {return -D3_asin(x);}
inline double D4_acos(double x) {return -D4_asin(x);}

inline double D0_atan(double x) {return std::atan(x);}
inline double D1_atan(double x) {return 1/(1+x*x);}
inline double D2_atan(double x) {return -2*x/pow((1+x*x),2);}
inline double D3_atan(double x) {return 2*(3*pow(x,2)-1)/pow(1+x*x,3);}
inline double D4_atan(double x) {return 24*x*(1-x*x)/pow(1+x*x,4);}

inline double D0_exp(double x) {return std::exp(x);}
inline double D1_exp(double x) {return std::exp(x);}
inline double D2_exp(double x) {return std::exp(x);}
inline double D3_exp(double x) {return std::exp(x);}
inline double D4_exp(double x) {return std::exp(x);}

inline double D0_log(double x) {return std::log(x);}
inline double D1_log(double x) {return 1/x;}
inline double D2_log(double x) {return -1/pow(x,2);}
inline double D3_log(double x) {return 2/pow(x,3);}
inline double D4_log(double x) {return -6/pow(x,4);}

typedef std::array<double(*)(double),5> frow_t;
static const std::map<f_t,frow_t> ftable {
    {f_t::sin_f, {D0_sin, D1_sin, D2_sin, D3_sin, D4_sin}},
    {f_t::cos_f, {D0_cos, D1_cos, D2_cos, D3_cos, D4_cos}},
    {f_t::sinh_f, {D0_sinh, D1_sinh, D2_sinh, D3_sinh, D4_sinh}},
    {f_t::cosh_f, {D0_cosh, D1_cosh, D2_cosh, D3_cosh, D4_cosh}},
    {f_t::asin_f, {D0_asin, D1_asin, D2_asin, D3_asin, D4_asin}},
    {f_t::acos_f, {D0_acos, D1_acos, D2_acos, D3_acos, D4_acos}},
    {f_t::atan_f, {D0_atan, D1_atan, D2_atan, D3_atan, D4_atan}},
    {f_t::exp_f, {D0_exp, D1_exp, D2_exp, D3_exp, D4_exp}},
    {f_t::log_f, {D0_log, D1_log, D2_log, D3_log, D4_log}}
};

template<int N>
class ad_var
{
    private:
        double v {1};
        std::array<double,N> d;

    public:
        ad_var() : d{0} {d[0] = 1;};
        ad_var(double x) : v(x), d{0} {d[0] = 1;};
        ad_var(const ad_var&) = default;
        ad_var(const ad_var<N+1>& vp) : d{0} {
            v = vp.get_var();
            for (int i=0; i<N; ++i)
                d[i] = vp.get(i);         
        };
        
        ad_var(const ad_var<N-1>& vp) : d{0} {
            v = vp.get_var();
            for (int i=0; i<N-1; ++i)
                d[i] = vp.get(i);         
        };
        
        ad_var(const std::initializer_list<double> l) {
            if (l.size() != N+1)
                throw std::runtime_error("ilist size mismatch");

            auto it = std::begin(l);
            v = *(it++);
            int i = 0;
            for(; it != std::end(l); ++it)
                d[i++] = *it;
        }
        
        std::string as_string() const {
            std::stringstream os;
            os << '(' << v << ':';
            auto cit = d.cbegin();
            for (; cit != d.cend()-1; ++cit)
                os << *cit << ',';
            os << *cit << ')';
            return os.str();
        }
        
        void set_var(double x) {
            v = x;
        }
        
        void set(int i, double x) {
            d[i] = x;
        }
        
        double get_var() const {
            return v;
        }
       
        double get(int i) const {
            return d[i];
        }
        
        const std::array<double,N>& get() const {
           return d; 
        }
        
        ad_var<N> operator*(double x) const {
            ad_var<N> a;
            a.v = x*v;
            for (int i=0; i < N; ++i)
                a.d[i] = x*d[i];
            return a;
        }
        
        ad_var<N> operator/(double x) const {
            ad_var<N> a;
            a.v = v/x;
            for (int i=0; i < N; ++i)
                a.d[i] = d[i]/x;
            return a;
        }
        
        ad_var<N> operator+(double x) const {
            ad_var<N> a;
            a.v = x + v;
            a.d = d;
            return a;
        }
        
        ad_var<N> operator-(double x) const {
            ad_var<N> a;
            a.v = v - x;
            a.d = d;
            return a;
        }
       
        ad_var<N> operator-() const {
            ad_var<N> a;
            a.v = -v;
            for (int i=0; i < N; ++i)
                a.d[i] = -d[i];
            return a;
        }
        
        ad_var<N> operator+(const ad_var<N>& b) const {
            ad_var<N> a;
            a.v = v + b.v;
            for (int i=0; i < N; ++i)
                a.d[i] = d[i] + b.d[i];
            return a;
        }
        
        bool operator==(const ad_var<N>& z) const {
            if (std::abs(v - z.v) > 1.0e-6)
                return false;
            for (int i=0; i<N; ++i)
                if (std::abs(d[i] - z.get(i)) > 1.0e-6)
                    return false;
            return true;
        }
};

// ============================================================================

template<int N>
inline ad_var<N> operator+(double x, const ad_var<N>& av)
{
    return av + x;
};

template<int N>
inline ad_var<N> operator-(double x, const ad_var<N>& av)
{
    return -(av - x);
};

template<int N>
inline ad_var<N> operator*(const ad_var<N>& av, double x)
{
    return ad_var<N>(av*x);
};

template<int N>
inline ad_var<N> operator*(double x, const ad_var<N>& av)
{
    return ad_var<N>(av*x);
};

// ==== generic functions ==================================================

template<int N>
inline ad_var<N> generic_f(const ad_var<N>&, f_t) = delete;

template<>
inline ad_var<1> generic_f(const ad_var<1>& av, f_t f)
{
    const auto& frow = ftable.at(f);
    ad_var<1> a(av);
    double x = av.get_var();
    a.set_var(frow.at(0)(x));
    auto f1 = frow.at(1)(x);
    auto g1 = av.get(0);
    a.set(0,f1*g1);
    return a;
}

template<>
inline ad_var<2> generic_f(const ad_var<2>& av, f_t f)
{
    const auto& frow = ftable.at(f);

    ad_var<2> a(generic_f(ad_var<1>(av),f));

    double x = av.get_var();
    auto f1 = frow.at(1)(x);
    auto f2 = frow.at(2)(x);
    const auto& g1 = av.get(0);
    const auto& g2 = av.get(1);
    a.set(1,f2*pow(g1,2)+f1*g2);
    return a;
}

template<>
inline ad_var<3> generic_f(const ad_var<3>& av, f_t f)
{
    const auto& frow = ftable.at(f);

    ad_var<3> a(generic_f(ad_var<2>(av),f));

    double x = av.get_var();
    auto f1 = frow.at(1)(x);
    auto f2 = frow.at(2)(x);
    auto f3 = frow.at(3)(x);
    const auto& g1 = av.get(0);
    const auto& g2 = av.get(1);
    const auto& g3 = av.get(2);
    a.set(2,g3*f1 + 3*g1*g2*f2 + pow(g1,3)*f3);
    return a;
}

template<>
inline ad_var<4> generic_f(const ad_var<4>& av, f_t f)
{
    const auto& frow = ftable.at(f);

    ad_var<4> a(generic_f(ad_var<3>(av),f));

    double x = av.get_var();
    auto f1 = frow.at(1)(x);
    auto f2 = frow.at(2)(x);
    auto f3 = frow.at(3)(x);
    auto f4 = frow.at(4)(x);
    const auto& g1 = av.get(0);
    const auto& g2 = av.get(1);
    const auto& g3 = av.get(2);
    const auto& g4 = av.get(3);
    a.set(3,g4*f1 + 4*g3*g1*f2 + 3*pow(g2,2)*f2 + 6*pow(g1,2)*g2*f3 + pow(g1,4)*f4);
    return a;
}

// =========================================================================

template<int N, int R>
inline constexpr int nCr() {
    static_assert(N >= R && N <= 4,"Not yet supported");
    return 1;
}

template<> inline constexpr int nCr<2,0>() {return 1;}
template<> inline constexpr int nCr<2,1>() {return 2;}
template<> inline constexpr int nCr<2,2>() {return 1;}

template<> inline constexpr int nCr<3,0>() {return 1;}
template<> inline constexpr int nCr<3,1>() {return 3;}
template<> inline constexpr int nCr<3,2>() {return 3;}
template<> inline constexpr int nCr<3,3>() {return 1;}

template<> inline constexpr int nCr<4,0>() {return 1;}
template<> inline constexpr int nCr<4,1>() {return 4;}
template<> inline constexpr int nCr<4,2>() {return 6;}
template<> inline constexpr int nCr<4,3>() {return 4;}
template<> inline constexpr int nCr<4,4>() {return 1;}

// ==== pow =============================================================

template<int N>
inline ad_var<N> pow(const ad_var<N>& av, double n) = delete;

template<>
inline ad_var<1> pow(const ad_var<1>& av, double n)
{
    ad_var<1> a;
    double x = av.get_var();
    a.set_var(pow(x,n));
    a.set(0,n*pow(x,n-1)*av.get(0));
    return a;
};

template<>
inline ad_var<2> pow(const ad_var<2>& av, double n)
{
    ad_var<2> a;
    double x = av.get_var();
    a.set_var(pow(x,n));
    auto g1 = n*pow(x,n-1);
    auto g2 = n*(n-1)*pow(x,n-2);
    const auto& f1 = av.get(0);
    const auto& f2 = av.get(1);
    a.set(0,g1*av.get(0));
    a.set(1,g2*pow(f1,2)+g1*f2);
    return a;
};

template<>
inline ad_var<3> pow(const ad_var<3>& av, double n)
{
    ad_var<3> a;
    double x = av.get_var();
    a.set_var(pow(x,n));
    
    auto f1 = n*pow(x,n-1);
    auto f2 = n*(n-1)*pow(x,n-2);
    auto f3 = n*(n-1)*(n-2)*pow(x,n-3);
    const auto& g1 = av.get(0);
    const auto& g2 = av.get(1);
    const auto& g3 = av.get(2);
    a.set(0,g1*f1);
    a.set(1,g2*f1+g1*g1*f2);
    a.set(2,g3*f1 + 3*g1*g2*f2 + pow(g1,3)*f3);
    return a;
};

template<>
inline ad_var<4> pow(const ad_var<4>& av, double n)
{
    ad_var<4> a;
    double x = av.get_var();
    a.set_var(pow(x,n));
    
    auto f1 = n*pow(x,n-1);
    auto f2 = n*(n-1)*pow(x,n-2);
    auto f3 = n*(n-1)*(n-2)*pow(x,n-3);
    auto f4 = n*(n-1)*(n-2)*(n-3)*pow(x,n-4);
    const auto& g1 = av.get(0);
    const auto& g2 = av.get(1);
    const auto& g3 = av.get(2);
    const auto& g4 = av.get(3);
    a.set(0,g1*f1);
    a.set(1,g2*f1+g1*g1*f2);
    a.set(2,g3*f1 + 3*g1*g2*f2 + pow(g1,3)*f3);
    a.set(3,g4*f1 + 4*g3*g1*f2 + 3*pow(g2,2)*f2 + 6*pow(g1,2)*g2*f3 + pow(g1,4)*f4);
    return a;
};

// ==== tan ==================================================================

template<int N>
inline ad_var<N> tan(const ad_var<N>& av)
{
    return sin(av)/cos(av);
}

// ==== tanh =================================================================

template<int N>
inline ad_var<N> tanh(const ad_var<N>& av)
{
    return sinh(av)/cosh(av);
}

// ==== sqrt ==================================================================

template<int N>
inline ad_var<N> sqrt(const ad_var<N>& av)
{
    return pow(av,0.5);
}

// ==== cbrt ==================================================================

template<int N>
inline ad_var<N> cbrt(const ad_var<N>& av)
{
    static constexpr double one_third = 1.0/3;
    return pow(av,one_third);
}

// ==== log ===================================================================

template<int N>
inline ad_var<N> log(const ad_var<N>& av)
{
    return generic_f(av, f_t::log_f);
};

// ==== log10 =================================================================

template<int N>
inline ad_var<N> log10(const ad_var<N>& av)
{
    static constexpr double log_10 = log(10);
    return log(av)/log_10;
};

// ==== log2 ==================================================================

template<int N>
inline ad_var<N> log2(const ad_var<N>& av)
{
    static constexpr double log_2 = log(2);
    return log(av)/log_2;
};

// ==== exp ===================================================================

template<int N>
inline ad_var<N> exp(const ad_var<N>& av)
{
    return generic_f(av, f_t::exp_f);
};

// ==== exp2 ===================================================================

template<int N>
inline ad_var<N> exp2(const ad_var<N>& av)
{
    return exp(av*log(2));    
}

// ==== sin ===================================================================

template<int N>
inline ad_var<N> sin(const ad_var<N>& av)
{
    return generic_f(av, f_t::sin_f);
}

// ==== cos ===================================================================

template<int N>
inline ad_var<N> cos(const ad_var<N>& av)
{
    return generic_f(av, f_t::cos_f);
}

// ==== asin ===================================================================

template<int N>
inline ad_var<N> asin(const ad_var<N>& av)
{
    return generic_f(av, f_t::asin_f);
}

// ==== acos ===================================================================

template<int N>
inline ad_var<N> acos(const ad_var<N>& av)
{
    return generic_f(av, f_t::acos_f);
}

// ==== atan ===================================================================

template<int N>
inline ad_var<N> atan(const ad_var<N>& av)
{
    return generic_f(av, f_t::atan_f);
}

// ==== sinh ==================================================================

template<int N>
inline ad_var<N> sinh(const ad_var<N>& av)
{
    return generic_f(av, f_t::sinh_f);
}

// ==== cosh ==================================================================

template<int N>
inline ad_var<N> cosh(const ad_var<N>& av)
{
    return generic_f(av, f_t::cosh_f);
}

// ==== operator* =============================================================
template<int N>
inline ad_var<N> operator*(const ad_var<N>& a, const ad_var<N>& b) = delete;

template<>
inline ad_var<4> operator*(const ad_var<4>& a, const ad_var<4>& b)
{
    ad_var<4> result;
    auto a_v = a.get_var();
    auto b_v = b.get_var();
    const auto& a_d = a.get();
    const auto& b_d = b.get();
    result.set_var(a_v*b_v);
    result.set(0,a_d[0]*b_v+a_v*b_d[0]);
    result.set(1,a_d[1]*b_v+nCr<2,1>()*a_d[0]*b_d[0]+a_v*b_d[1]);
    result.set(2,a_d[2]*b_v+nCr<3,1>()*a_d[1]*b_d[0]+nCr<3,2>()*a_d[0]*b_d[1]+a_v*b_d[2]);
    result.set(3,a_d[3]*b_v+nCr<4,1>()*a_d[2]*b_d[0]+nCr<4,2>()*a_d[1]*b_d[1]+nCr<4,3>()*a_d[0]*b_d[2]+a_v*b_d[3]);
    return result;
};

template<>
inline ad_var<3> operator*(const ad_var<3>& a, const ad_var<3>& b)
{
    ad_var<3> result;
    auto a_v = a.get_var();
    auto b_v = b.get_var();
    const auto& a_d = a.get();
    const auto& b_d = b.get();
    result.set_var(a_v*b_v);
    result.set(0,a_d[0]*b_v+a_v*b_d[0]);
    result.set(1,a_d[1]*b_v+nCr<2,1>()*a_d[0]*b_d[0]+a_v*b_d[1]);
    result.set(2,a_d[2]*b_v+nCr<3,1>()*a_d[1]*b_d[0]+nCr<3,2>()*a_d[0]*b_d[1]+a_v*b_d[2]);
    return result;
};

template<>
inline ad_var<2> operator*(const ad_var<2>& a, const ad_var<2>& b)
{
    ad_var<2> result;
    auto a_v = a.get_var();
    auto b_v = b.get_var();
    const auto& a_d = a.get();
    const auto& b_d = b.get();
    result.set_var(a_v*b_v);
    result.set(0,a_d[0]*b_v+a_v*b_d[0]);
    result.set(1,a_d[1]*b_v+nCr<2,1>()*a_d[0]*b_d[0]+a_v*b_d[1]);
   return result;
};

template<>
inline ad_var<1> operator*(const ad_var<1>& a, const ad_var<1>& b)
{
    ad_var<1> result;
    auto a_v = a.get_var();
    auto b_v = b.get_var();
    const auto& a_d = a.get();
    const auto& b_d = b.get();
    result.set_var(a_v*b_v);
    result.set(0,a_d[0]*b_v+a_v*b_d[0]);
    return result;
};

template<int N>
inline ad_var<N> operator/(const ad_var<N>& a, const ad_var<N>& b)
{
    return a*pow(b,-1);
}

template<int N>
inline ad_var<N> operator/(double x, const ad_var<N>& b)
{
    return x*pow(b,-1);
}
// ============================================================================

#endif
