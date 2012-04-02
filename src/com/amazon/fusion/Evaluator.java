// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonException;
import com.amazon.ion.IonList;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;

/**
 * Main entry point to the Fusion evaluation engine.
 */
final class Evaluator
{
    static IonValue cloneIfContained(IonValue value)
    {
        if (value.getContainer() != null)
        {
            value = value.clone();
        }
        return value;
    }


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
            {
                return eval(env, (IonList) expr);
            }
            case STRUCT:
            {
                return eval(env, (IonStruct) expr);
            }
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

    FusionValue eval(Environment env, IonList expr)
    {
        IonList resultDom;
        if (expr.isNullValue())
        {
            resultDom = expr;
        }
        else
        {
            ValueFactory vf = expr.getSystem();
            resultDom = vf.newEmptyList();
            for (IonValue elementExpr : expr)
            {
                DomValue elementValue = (DomValue) eval(env, elementExpr);
                IonValue elementDom = elementValue.getDom();
                elementDom = cloneIfContained(elementDom);
                resultDom.add(elementDom);
            }
        }
        return new DomValue(resultDom);
    }

    FusionValue eval(Environment env, IonStruct expr)
    {
        IonStruct resultDom;
        if (expr.isNullValue())
        {
            resultDom = expr;
        }
        else
        {
            ValueFactory vf = expr.getSystem();
            resultDom = vf.newEmptyStruct();
            for (IonValue elementExpr : expr)
            {
                DomValue elementValue = (DomValue) eval(env, elementExpr);
                IonValue elementDom = elementValue.getDom();
                elementDom = cloneIfContained(elementDom);
                resultDom.add(elementExpr.getFieldName(), elementDom);
            }
        }
        return new DomValue(resultDom);
    }
}
