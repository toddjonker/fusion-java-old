// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * The {@code lambda} syntactic form, which evaluates to a {@link Closure}.
 */
final class LambdaKeyword
    extends KeywordValue
{
    LambdaKeyword()
    {
        //    "                                                                               |
        super("(PARAM ...) DOC? BODY",
              "Returns a new procedure. When invoked, the caller's arguments are bound to the\n" +
              "PARAMs and the BODY is evaluated and returned.\n" +
              "DOC is an optional documentation string.\n" +
              "BODY may be one or more forms; the result of the last form is the result of the\n" +
              "procedure invocation.");
    }


    @Override
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp expr)
        throws SyntaxFailure
    {
        int bodyStart;

        SyntaxValue maybeDoc = expr.get(2);
        if (maybeDoc.getType() == SyntaxValue.Type.STRING
            && expr.size() > 3)
        {
            bodyStart = 3;
        }
        else
        {
            bodyStart = 2;
        }

        String[] params = determineParams((SyntaxSexp) expr.get(1));
        int paramCount = params.length;

        Environment bodyEnv;
        if (paramCount == 0)
        {
            bodyEnv = env;
        }
        else
        {
            bodyEnv =
                new LocalEnvironment(env, params, new FusionValue[paramCount]);
        }

        // We create a wrap even if there's no params, because there may be
        // local definitions that will be added to the wrap.
        SyntaxWrap localWrap = new EnvironmentRenameWrap(bodyEnv);

        final int bodyEnd = expr.size();
        for (int i = bodyStart; i < bodyEnd; i++)
        {
            SyntaxValue bodyForm = expr.get(i);
            bodyForm.addWrap(localWrap);

            SyntaxValue expanded = bodyForm.prepare(eval, bodyEnv);
            if (expanded != bodyForm) expr.set(i, expanded);
        }
        return expr;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
    {
        String doc;
        int bodyStart;

        SyntaxValue maybeDoc = expr.get(2);
        if (maybeDoc.getType() == SyntaxValue.Type.STRING
            && expr.size() > 3)
        {
            doc = ((SyntaxString) maybeDoc).stringValue();
            if (doc != null) doc = doc.trim();
            bodyStart = 3;
        }
        else
        {
            doc = null;
            bodyStart = 2;
        }

        String[] params = determineParams((SyntaxSexp) expr.get(1));
        return new Closure(env, expr, doc, params, bodyStart);
    }

    private static String[] determineParams(SyntaxSexp paramsExpr)
    {
        int size = paramsExpr.size();
        String[] params = new String[size];
        for (int i = 0; i < size; i++)
        {
            // TODO typecheck
            SyntaxSymbol param = (SyntaxSymbol) paramsExpr.get(i);
            params[i] = param.stringValue();
        }
        return params;
    }
}
