// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonException;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;

/**
 * Main entry point to the Fusion evaluation engine.
 */
final class Evaluator
{
    IonValue evalToIon(Environment env, IonValue expr)
    {
        FusionValue fv = eval(env, expr);
        return fv.getDom();
    }


    FusionValue eval(Environment env, IonValue expr)
    {
        switch (expr.getType())
        {
            case BLOB:
            case BOOL:
            case CLOB:
            case DECIMAL:
            case FLOAT:
            case INT:
            case NULL:
            case STRING:
            case TIMESTAMP:
                return new DomValue(expr);
            case LIST:
            case STRUCT:
                break;
            case SEXP:
            {
                return eval(env, (IonSexp) expr);
            }
            case SYMBOL:
            {
                return eval(env, (IonSymbol) expr);
            }
            case DATAGRAM:
                throw new IllegalStateException("Shouldn't have datagram here");
        }

        return new DomValue(expr);
    }


    FusionValue eval(Environment env, IonSymbol expr)
    {
        String name = expr.stringValue();
        FusionValue result = env.lookup(name);
        if (result == null)
        {
            throw new IonException("No binding for " + name);
        }
        return result;
    }


    FusionValue eval(Environment env, IonSexp expr)
    {
        int len = expr.size();
        if (len < 1) return null;

        IonValue first = expr.get(0);

        FusionValue form = eval(env, first);
        if (form == null)
        {
            throw new IonException("Bad form: " + first);
        }

        return form.invoke(this, env, expr);
    }
}
