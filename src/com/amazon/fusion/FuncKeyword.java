// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonString;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;

/**
 * The {@code func} syntactic form, which evaluates to a {@link FuncValue}.
 */
final class FuncKeyword
    extends KeywordValue
{
    FuncKeyword()
    {
        //    "                                                                               |
        super("(PARAM ...) DOC? BODY",
              "Returns a new function.  When invoked, the caller's arguments are bound to the\n" +
              "PARAMs and the BODY is evaluated and returned.\n" +
              "DOC is an optional documentation string.\n" +
              "BODY may be one or more forms; the result of the last form is the result of the\n" +
              "function invocation.");
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
    {
        String doc;
        int bodyStart;

        IonValue maybeDoc = expr.get(2);
        if (maybeDoc.getType() == IonType.STRING
            && expr.size() > 3)
        {
            doc = ((IonString) maybeDoc).stringValue();
            if (doc != null) doc = doc.trim();
            bodyStart = 3;
        }
        else
        {
            doc = null;
            bodyStart = 2;
        }

        return new FuncValue(env, expr, doc, bodyStart);
    }
}
