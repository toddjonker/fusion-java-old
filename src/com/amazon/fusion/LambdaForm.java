// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionList.immutableList;

/**
 * The {@code lambda} syntactic form, which evaluates to a {@link Closure}.
 */
final class LambdaForm
    extends SyntacticForm
{
    LambdaForm()
    {
        //    "                                                                               |
        super("(ARG ...) DOC? BODY ...+",
              "Returns a new procedure. When invoked, the caller's arguments are bound to the\n" +
              "ARG identifiers and the BODY is evaluated and returned.\n" +
              "DOC is an optional documentation string.\n" +
              "BODY may be one or more forms; the result of the last form is the result of the\n" +
              "procedure invocation.\n" +
              "\n" +
              "    (lambda REST_ID DOC? BODY ...+)\n" +
              "\n" +
              "This variant returns a procedure that accepts any number of arguments, which\n" +
              "are collected into an immutable sequence and bound to `rest_id`.");
    }


    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxChecker check = check(stx);
        int arity = check.arityAtLeast(3);

        SyntaxValue[] children = stx.extract();

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

        boolean isRest = (children[1] instanceof SyntaxSymbol);

        SyntaxSymbol[] args;
        if (isRest)
        {
            // Check for null/empty symbol
            SyntaxSymbol rest = check.requiredIdentifier("rest parameter", 1);
            args = new SyntaxSymbol[]{ rest };
        }
        else
        {
            SyntaxChecker checkFormals =
                check.subformSexp("formal arguments", 1);
            args = determineArgs(checkFormals);
        }

        // We create a wrap even if there's no arguments, because there may be
        // local definitions that will be added to the wrap.
        Environment bodyEnv = new LocalEnvironment(env, args);
        SyntaxWrap localWrap = new EnvironmentRenameWrap(bodyEnv);

        // Prepare the bound names so they resolve to their own binding.
        for (int i = 0; i < args.length; i++)
        {
            SyntaxSymbol arg = args[i];
            arg = arg.addWrap(localWrap);
            arg.resolve();           // Caches the binding in the identifier
            args[i] = arg;
        }

        children[1] = (isRest
                          ? args[0]
                          : SyntaxSexp.make(expander,
                                            children[1].getLocation(), args));

        // TODO FUSION-36 Should allow internal definitions
        for (int i = bodyStart; i < children.length; i++)
        {
            SyntaxValue bodyForm = children[i];
            bodyForm = bodyForm.addWrap(localWrap);
            bodyForm = expander.expandExpression(bodyEnv, bodyForm);
            children[i] = bodyForm;
        }

        stx = SyntaxSexp.make(expander, stx.getLocation(), children);
        return stx;
    }


    private static SyntaxSymbol[] determineArgs(SyntaxChecker checkArgs)
        throws SyntaxFailure
    {
        SyntaxSexp argSexp = (SyntaxSexp) checkArgs.form();
        int size = argSexp.size();
        if (size == 0) return SyntaxSymbol.EMPTY_ARRAY;

        SyntaxSymbol[] args = new SyntaxSymbol[size];
        for (int i = 0; i < size; i++)
        {
            args[i] = checkArgs.requiredIdentifier("formal argument name", i);
        }
        return args;
    }

    //========================================================================


    private static String[] determineArgNames(SyntaxSexp argSexp)
    {
        int size = argSexp.size();
        if (size == 0) return FusionUtils.EMPTY_STRING_ARRAY;

        String[] args = new String[size];
        for (int i = 0; i < size; i++)
        {
            SyntaxSymbol identifier = (SyntaxSymbol) argSexp.get(i);
            Binding binding = identifier.resolve();
            args[i] = binding.getName();
        }
        return args;
    }


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        String doc;
        int bodyStart;

        {
            SyntaxValue maybeDoc = stx.get(2);
            if (maybeDoc.getType() == SyntaxValue.Type.STRING
                && stx.size() > 3)
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
        }

        // Dummy environment to keep track of depth
        env = new LocalEnvironment(env, SyntaxSymbol.EMPTY_ARRAY);

        CompiledForm body = BeginForm.compile(eval, env, stx, bodyStart);

        boolean isRest = (stx.get(1) instanceof SyntaxSymbol);
        if (isRest)
        {
            SyntaxSymbol identifier = (SyntaxSymbol) stx.get(1);
            Binding binding = identifier.getBinding();
            return new CompiledLambdaRest(doc, binding.getName(), body);
        }
        else
        {
            String[] argNames = determineArgNames((SyntaxSexp) stx.get(1));
            switch (argNames.length)
            {
                case 1:
                    return new CompiledLambda1(doc, argNames, body);
                case 2:
                    return new CompiledLambda2(doc, argNames, body);
                default:
                    return new CompiledLambdaN(doc, argNames, body);
            }
        }
    }


    //========================================================================


    abstract static class CompiledLambdaBase
        implements CompiledForm
    {
        final String       myDoc;
        final String[]     myArgNames;
        final CompiledForm myBody;

        CompiledLambdaBase(String doc, String[] argNames, CompiledForm body)
        {
            myDoc      = doc;
            myArgNames = argNames;
            myBody     = body;
        }
    }

    /** Marker for lambdas that accept a fixed number of arguments. */
    interface CompiledLambdaExact
    {
    }

    private static class CompiledLambdaN
        extends CompiledLambdaBase
        implements CompiledLambdaExact
    {
        CompiledLambdaN(String doc, String[] argNames, CompiledForm body)
        {
            super(doc, argNames, body);
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            return new Closure(store, myDoc, myArgNames, myBody);
        }
    }


    private static class Closure
        extends Procedure
    {
        final Store        myEnclosure;
        final CompiledForm myBody;

        /**
         * Constructs a new closure from its source and enclosing lexical
         * environment.
         *
         * @param enclosure the store lexically surrounding the source of this
         *  closure.  Any free variables in the procedure are expected to be
         *  bound here.
         */
        Closure(Store enclosure, String doc, String[] argNames,
                CompiledForm body)
        {
            super(doc, argNames);

            myEnclosure = enclosure;
            myBody      = body;
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            Store localStore = new LocalStore(myEnclosure, args);

            return eval.bounceTailForm(localStore, myBody);
        }
    }


    //========================================================================


    private static final class CompiledLambda1
        extends CompiledLambdaBase
        implements CompiledLambdaExact
    {
        CompiledLambda1(String doc, String[] argNames, CompiledForm body)
        {
            super(doc, argNames, body);
            assert argNames.length == 1;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            return new Closure1(store, myDoc, myArgNames, myBody);
        }
    }


    private static final class Closure1
        extends Closure
    {
        Closure1(Store enclosure, String doc, String[] argNames,
                 CompiledForm body)
        {
            super(enclosure, doc, argNames, body);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(1, args);

            Store localStore = new LocalStore1(myEnclosure, args[0]);

            return eval.bounceTailForm(localStore, myBody);
        }
    }


    //========================================================================


    private static final class CompiledLambda2
        extends CompiledLambdaBase
        implements CompiledLambdaExact
    {
        CompiledLambda2(String doc, String[] argNames, CompiledForm body)
        {
            super(doc, argNames, body);
            assert argNames.length == 2;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            return new Closure2(store, myDoc, myArgNames, myBody);
        }
    }


    private static final class Closure2
        extends Closure
    {
        Closure2(Store enclosure, String doc, String[] argNames,
                 CompiledForm body)
        {
            super(enclosure, doc, argNames, body);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(2, args);

            Store localStore = new LocalStore2(myEnclosure, args[0], args[1]);

            return eval.bounceTailForm(localStore, myBody);
        }
    }


    //========================================================================


    private static final class CompiledLambdaRest
        extends CompiledLambdaBase
    {
        CompiledLambdaRest(String doc, String restArgName, CompiledForm body)
        {
            super(doc, new String[]{ restArgName, Procedure.DOTDOTDOT }, body);
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            return new ClosureRest(store, myDoc, myArgNames, myBody);
        }
    }


    private static final class ClosureRest
        extends Closure
    {
        ClosureRest(Store enclosure, String doc, String[] argNames,
                    CompiledForm body)
        {
            super(enclosure, doc, argNames, body);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            Object rest = immutableList(eval, args);

            Store localStore = new LocalStore1(myEnclosure, rest);

            return eval.bounceTailForm(localStore, myBody);
        }
    }
}
