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
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp source)
        throws SyntaxFailure
    {
        SyntaxChecker check = check(source);
        int arity = check.arityAtLeast(3);

        SyntaxValue[] children = source.extract();

        int bodyStart;
        SyntaxValue maybeDoc = children[2];
        if (maybeDoc.getType() == SyntaxValue.Type.STRING && arity > 3)
        {
            bodyStart = 3;
        }
        else
        {
            bodyStart = 2;
        }

        check.requiredSexp("formal parameters", 1);
        SyntaxSymbol[] params = determineParams((SyntaxSexp) children[1]);

        // We create a wrap even if there's no params, because there may be
        // local definitions that will be added to the wrap.
        Environment bodyEnv = new LocalEnvironment(env, params);
        SyntaxWrap localWrap = new EnvironmentRenameWrap(bodyEnv);

        // Prepare the bound names so they resolve to their own binding.
        for (int i = 0; i < params.length; i++)
        {
            SyntaxSymbol param = params[i];
            param = param.addWrap(localWrap);
            param.resolve();           // Caches the binding in the identifier
            params[i] = param;
        }

        children[1] = SyntaxSexp.make(children[1].getLocation(), params);

        for (int i = bodyStart; i < children.length; i++)
        {
            SyntaxValue bodyForm = children[i];
            bodyForm = bodyForm.addWrap(localWrap);
            bodyForm = bodyForm.prepare(eval, bodyEnv);
            children[i] = bodyForm;
        }

        source = SyntaxSexp.make(source.getLocation(), children);
        return source;
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

        SyntaxSymbol[] params = determineParams((SyntaxSexp) expr.get(1));
        return new Closure(env, expr, doc, params, bodyStart);
    }

    private static SyntaxSymbol[] determineParams(SyntaxSexp paramsExpr)
    {
        int size = paramsExpr.size();
        if (size == 0) return SyntaxSymbol.EMPTY_ARRAY;

        SyntaxSymbol[] params = new SyntaxSymbol[size];
        for (int i = 0; i < size; i++)
        {
            // TODO typecheck
            params[i] = (SyntaxSymbol) paramsExpr.get(i);
        }
        return params;
    }
}
