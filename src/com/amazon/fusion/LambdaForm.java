// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * The {@code lambda} syntactic form, which evaluates to a {@link Closure}.
 */
final class LambdaForm
    extends SyntacticForm
{
    LambdaForm()
    {
        //    "                                                                               |
        super("(ARG ...) DOC? BODY",
              "Returns a new procedure. When invoked, the caller's arguments are bound to the\n" +
              "ARG identifiers and the BODY is evaluated and returned.\n" +
              "DOC is an optional documentation string.\n" +
              "BODY may be one or more forms; the result of the last form is the result of the\n" +
              "procedure invocation.");
    }


    @Override
    SyntaxValue expand(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
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

        SyntaxChecker checkFormals = check.subformSexp("formal arguments", 1);
        SyntaxSymbol[] args = determineArgs(checkFormals);

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

        children[1] = SyntaxSexp.make(children[1].getLocation(), args);

        for (int i = bodyStart; i < children.length; i++)
        {
            SyntaxValue bodyForm = children[i];
            bodyForm = bodyForm.addWrap(localWrap);
            bodyForm = bodyForm.expand(eval, bodyEnv);
            children[i] = bodyForm;
        }

        source = SyntaxSexp.make(source.getLocation(), children);
        return source;
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
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        String doc;
        int bodyStart;

        {
            SyntaxValue maybeDoc = source.get(2);
            if (maybeDoc.getType() == SyntaxValue.Type.STRING
                && source.size() > 3)
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

        CompiledForm body = BeginForm.compile(eval, env, source, bodyStart);

        String[] argNames = determineArgNames((SyntaxSexp) source.get(1));
        return new CompiledLambda(doc, argNames, body);
    }


    //========================================================================


    private static final class CompiledLambda
        implements CompiledForm
    {
        private final String       myDoc;
        private final String[]     myArgNames;
        private final CompiledForm myBody;

        CompiledLambda(String doc, String[] argNames, CompiledForm body)
        {
            myDoc      = doc;
            myBody     = body;
            myArgNames = argNames;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            return new Closure(store, myDoc, myArgNames, myBody);
        }
    }


    private static final class Closure
        extends Procedure
    {
        private final Store        myEnclosure;
        private final CompiledForm myBody;


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
        Object doApply(Evaluator eval, final Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            Store localStore = new LocalStore(myEnclosure, args);

            return eval.bounceTailForm(localStore, myBody);
        }
    }
}
