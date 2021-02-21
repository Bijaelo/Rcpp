//
// ArgumentList.h: Rcpp R/C++ interface class library -- ArgumentList
//
// Copyright (C) 2020-  Dirk Eddelbuettel, Romain Francois and Oliver Per Madsen
//
// This file is part of Rcpp.
//
// Rcpp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// Rcpp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Rcpp.  If not, see <http://www.gnu.org/licenses/>.

// ArgumentList is identical to Vector in all but being a Vector. Useful for
// the implementation of do_call.
#ifndef Rcpp__argumentlist__argumentlist_h
#define Rcpp__argumentlist__argumentlist_h

#include <Rcpp/vector/Vector.h>
#include <Rcpp/vector/Subsetter.h>

namespace Rcpp{

template <int RTYPE, template <class> class StoragePolicy = PreserveStorage >
class ArgumentList_Impl : public Vector<RTYPE, StoragePolicy>{
public:
    typename traits::r_vector_cache_type<RTYPE, StoragePolicy>::type cache ;
    typedef typename traits::r_vector_proxy<RTYPE, StoragePolicy>::type Proxy ;
    typedef typename traits::r_vector_const_proxy<RTYPE, StoragePolicy>::type const_Proxy ;
    typedef typename traits::r_vector_name_proxy<RTYPE, StoragePolicy>::type NameProxy ;
    typedef typename traits::r_vector_proxy<RTYPE, StoragePolicy>::type value_type ;
    typedef typename traits::r_vector_iterator<RTYPE, StoragePolicy>::type iterator ;
    typedef typename traits::r_vector_const_iterator<RTYPE, StoragePolicy>::type const_iterator ;
    typedef typename traits::init_type<RTYPE>::type init_type ;
    typedef typename traits::r_vector_element_converter<RTYPE>::type converter_type ;
    typedef typename traits::storage_type<RTYPE>::type stored_type ;

    ArgumentList_Impl() : Vector<RTYPE, StoragePolicy>(){};


    // Maybe this should not call vector but ArgumentList<RTYPE, StoragePolicy> ?
    // It is not completely clear
    ArgumentList_Impl& operator=(const Vector<RTYPE, StoragePolicy>& rhs){
        return Vector<RTYPE, StoragePolicy>::operator=(rhs);
    };
    ArgumentList_Impl(SEXP x) : Vector<RTYPE, StoragePolicy>(x){};
    template<typename Proxy>
    ArgumentList_Impl(const GenericProxy<Proxy>& proxy) : Vector<RTYPE, StoragePolicy>(proxy){};
    ArgumentList_Impl(const no_init_vector& obj) : Vector<RTYPE, StoragePolicy>(obj){};
    template<typename T>

    ArgumentList_Impl(const T& size, const stored_type& u,
                      typename Rcpp::traits::enable_if<traits::is_arithmetic<T>::value, void>::type* = 0) :
        Vector<RTYPE, StoragePolicy>(size, u){}; // Should we add something extra for type*?

    ArgumentList_Impl(const int& size, const stored_type& u) : Vector<RTYPE, StoragePolicy>(size, u){};
    ArgumentList_Impl(const std::string& st) : Vector<RTYPE, StoragePolicy>(st){};
    ArgumentList_Impl(const char* st) : Vector<RTYPE, StoragePolicy>(st){};

    template <typename T>
    ArgumentList_Impl(const T& siz, stored_type (*gen)(void),
                      typename Rcpp::traits::enable_if<traits::is_arithmetic<T>::value, void>::type* = 0) :
        Vector<RTYPE, StoragePolicy>(siz, gen){}; // should we add something extra for type*?

    template<typename T>
    ArgumentList_Impl(T size,
                      typename Rcpp::traits::enable_if<traits::is_arithmetic<T>::value, void>::type* = 0) :
        Vector<RTYPE, StoragePolicy>(size){};// should we add something extra for type*?
    ArgumentList_Impl(const int& size) : Vector<RTYPE, StoragePolicy>(size){};
    ArgumentList_Impl(const Dimension& dims) : Vector<RTYPE, StoragePolicy>(dims){};

    template<typename T>
    ArgumentList_Impl(T value,
                      typename Rcpp::traits::enable_if<traits::is_bool<T>::value && RTYPE == LGLSXP, void>::type* = 0) :
        Vector<RTYPE, StoragePolicy>(value){};// should we add something extra for type*?

    template<typename U>
    ArgumentList_Impl(const Dimension& dims, const U& u) : Vector<RTYPE, StoragePolicy>(dims, u){};
    template<bool NA, typename VEC>
    ArgumentList_Impl(const VectorBase<RTYPE, NA, VEC>& other) : Vector<RTYPE, StoragePolicy>(other){};

    template<typename T, typename U>
    ArgumentList_Impl(const T& size, const U& u,
                      typename Rcpp::traits::enable_if<traits::is_arithmetic<T>::value, void>::type* = 0) :
        Vector<RTYPE, StoragePolicy>(size, u){}; // should we add something extra for type*?

    template <typename T, typename U1>
    ArgumentList_Impl(const T& siz, stored_type (*gen)(U1), const U1& u1,
                      typename Rcpp::traits::enable_if<traits::is_arithmetic<T>::value, void>::type* = 0) :
        Vector<RTYPE, StoragePolicy>(siz, gen, u1){}; // should we add something extra for type*?

    template <typename T, typename U1, typename U2>
    ArgumentList_Impl(const T& siz, stored_type (*gen)(U1,U2), const U1& u1, const U2& u2,
                      typename Rcpp::traits::enable_if<traits::is_arithmetic<T>::value, void>::type* = 0) :
        Vector<RTYPE, StoragePolicy>(siz, gen, u1, u2){};// should we add something extra for type*?

    template <typename T, typename U1, typename U2, typename U3>
    ArgumentList_Impl(const T& siz, stored_type (*gen)(U1,U2,U3), const U1& u1, const U2& u2, const U3& u3,
                      typename Rcpp::traits::enable_if<traits::is_arithmetic<T>::value, void>::type* = 0) :
        Vector<RTYPE, StoragePolicy>(siz, gen, u1, u2, u3){};// should we add something extra for type*?

    template <typename InputIterator>
    ArgumentList_Impl(InputIterator first, InputIterator last) : Vector<RTYPE, StoragePolicy>(first, last){};
    template <typename InputIterator, typename T>
    ArgumentList_Impl(InputIterator first, InputIterator last, T n,
                      typename Rcpp::traits::enable_if<traits::is_arithmetic<T>::value, void>::type* = 0) :
        Vector<RTYPE, StoragePolicy>(first, last, n){};// should we add something extra for type*?

    template <typename InputIterator, typename Func>
    ArgumentList_Impl(InputIterator first, InputIterator last, Func func) :
        Vector<RTYPE, StoragePolicy>(first, last, func){};

    template <typename InputIterator, typename Func, typename T>
    ArgumentList_Impl(InputIterator first, InputIterator last, Func func, T n,
                      typename Rcpp::traits::enable_if<traits::is_arithmetic<T>::value, void>::type* = 0) :
        Vector<RTYPE, StoragePolicy>(first ,last, func){}; // should we add something extra for type*?

#ifdef HAS_CXX0X_INITIALIZER_LIST
    ArgumentList_Impl(std::initializer_list<init_type> list) : Vector<RTYPE, StoragePolicy>(list){};
#endif


    // Maybe this should not call vector but ArgumentList<RTYPE, StoragePolicy> ?
    // It is not completely clear to me (Olvier)
    template <typename T>
    ArgumentList_Impl& operator=(const T& rhs){
      return Vector<RTYPE, StoragePolicy>::operator=(rhs);
    }
};

typedef ArgumentList_Impl<VECSXP> ArgumentList;
}
#endif
