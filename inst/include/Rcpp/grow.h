// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; tab-width: 8 -*-
//
// grow.h: Rcpp R/C++ interface class library -- grow a pairlist
//
// Copyright (C) 2010 - 2013 Dirk Eddelbuettel and Romain Francois
// Copyright (C) 2021 - 2021 Dirk Eddelbuettel, Romain Francois and Oliver Per Madsen
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

#ifndef Rcpp_grow_h
#define Rcpp_grow_h

#include <RcppCommon.h>
#include <Rcpp/Named.h>

namespace Rcpp {

    inline SEXP pairlist() {
        return R_NilValue ;
    }

    // Special handling for ArgumentList: Grow pairlist tail.
    inline SEXP pairlist(ArgumentList& t1){
        // code mostly taken from do_docall
        // Not sure why this one is complaining about it not being a union type?
        R_xlen_t n = t1.size();
        SEXP c, call;
        PROTECT(c = call = Rf_allocList(n));
        // t1.names() might return R_NilValue making Shield<SEXP> ambigous
        SEXP names;
        // PROTECT might be unnecessary, as attr are stored as Shield<SEXP>?
        // Better safe than sorry.
        PROTECT(names = t1.names());
        Rcpp::ArgumentList::iterator ti = t1.begin();
        if(Rf_length(names) == 0){
            for(R_xlen_t i = 0; i < n; i++, ti++){
                SETCAR(c, *ti);
                c = CDR(c);
            }
        }else{
            for(R_xlen_t i = 0; i < n; i++, ti++){
                SETCAR(c, *ti);
                // PROTECT not needed here
                SEXP namei = STRING_ELT(names, i);
                // Test for NULL and nullstring.
                if(namei != R_NilValue && CHAR(namei)[0] != '\0')
                    SET_TAG(c, Rf_installTrChar(namei));
                c = CDR(c);
            }
        }
        UNPROTECT(2);
        return(call);
    };

    inline SEXP grow( SEXP head, SEXP tail ) {
        Shield<SEXP> x( head ) ;
        Shield<SEXP> res( Rf_cons( x, tail ) ) ;
        return res ;
    }

    namespace internal{

        template <typename T>
        inline SEXP grow__dispatch( ::Rcpp::traits::false_type, const T& head, SEXP tail ){
            return grow( wrap(head), tail ) ;
        }

        template <typename T>
        inline SEXP grow__dispatch( ::Rcpp::traits::true_type, const T& head, SEXP tail ){
            Shield<SEXP> y( wrap( head.object) ) ;
            Shield<SEXP> x( Rf_cons( y , tail) ) ;
            SEXP headNameSym = ::Rf_install( head.name.c_str() );
            SET_TAG( x, headNameSym );
            return x;
        }

    } // internal

    /**
     * grows a pairlist. First wrap the head into a SEXP, then
     * grow the tail pairlist
     */
    template <typename T>
    SEXP grow(const T& head, SEXP tail) {
        Shield<SEXP> y(tail);
        return internal::grow__dispatch(typename traits::is_named<T>::type(), head, y);
    }

    inline SEXP grow( const char* head, SEXP tail ) {
        Shield<SEXP> y(tail);
        return grow(Rf_mkString(head), y);
    }

    #include <Rcpp/generated/grow__pairlist.h>

} // namespace Rcpp

#endif
