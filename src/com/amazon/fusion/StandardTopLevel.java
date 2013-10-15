// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.BindingDoc.COLLECT_DOCS_MARK;
import static com.amazon.fusion.FusionVoid.voidValue;
import static com.amazon.fusion.FusionWrite.safeWriteToString;
import static com.amazon.ion.util.IonTextUtils.printQuotedSymbol;
import static java.lang.Boolean.TRUE;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonSystem;
import java.io.File;


final class StandardTopLevel
    implements TopLevel
{
    private final Evaluator myEvaluator;
    private final Namespace myNamespace;


    /**
     * @param initialModulePath must be absolute.
     */
    StandardTopLevel(GlobalState globalState,
                     Namespace namespace,
                     String initialModulePath,
                     boolean documenting)
        throws FusionException
    {
        assert initialModulePath.startsWith("/");

        Evaluator eval = new Evaluator(globalState);
        if (documenting)
        {
            eval = eval.markedContinuation(COLLECT_DOCS_MARK, TRUE);
        }

        myEvaluator = eval;
        myNamespace = namespace;
        namespace.require(myEvaluator, initialModulePath);
    }

    /**
     * @param initialModulePath must be absolute.
     */
    StandardTopLevel(GlobalState globalState,
                     ModuleRegistry registry,
                     String initialModulePath)
        throws FusionException
    {
        this(globalState, new TopLevelNamespace(registry), initialModulePath,
             false);
    }


    //========================================================================

    // NOT PUBLIC
    Evaluator getEvaluator()
    {
        return myEvaluator;
    }


    @Override
    public Object eval(String source, SourceName name)
        throws ExitException, FusionException
    {
        IonSystem system = myEvaluator.getGlobalState().myIonSystem;
        IonReader i = system.newReader(source);
        return eval(i, name);
    }

    @Deprecated @Override
    public Object load(String source, SourceName name)
        throws ExitException, FusionException
    {
        return eval(source, name);
    }


    @Override
    public Object eval(String source)
        throws ExitException, FusionException
    {
        return eval(source, null);
    }

    @Deprecated @Override
    public Object load(String source)
        throws ExitException, FusionException
    {
        return eval(source, null);
    }


    @Override
    public Object eval(IonReader source, SourceName name)
        throws ExitException, FusionException
    {
        Object result = voidValue(myEvaluator);

        if (source.getType() == null) source.next();

        while (source.getType() != null)
        {
            SyntaxValue sourceExpr = Syntax.read(myEvaluator, source, name);
            result = FusionEval.eval(myEvaluator, sourceExpr, myNamespace);
            source.next();
        }

        return result;
    }

    @Deprecated @Override
    public Object load(IonReader source, SourceName name)
        throws ExitException, FusionException
    {
        return eval(source, name);
    }


    @Override
    public Object eval(IonReader source)
        throws ExitException, FusionException
    {
        return eval(source, null);
    }

    @Deprecated @Override
    public Object load(IonReader source)
        throws ExitException, FusionException
    {
        return eval(source, null);
    }


    @Override
    public Object load(File source)
        throws ExitException, FusionException
    {
        LoadHandler load = myEvaluator.getGlobalState().myLoadHandler;
        return load.loadTopLevel(myEvaluator, myNamespace, source.toString());
    }


    @Override
    public void requireModule(String modulePath)
        throws FusionException
    {
        myNamespace.require(myEvaluator, modulePath);
    }


    @Override
    public void define(String name, Object value)
        throws FusionException
    {
        Object fv = myEvaluator.injectMaybe(value);
        if (fv == null)
        {
            String expected =
                "injectable Java type but received " +
                value.getClass().getName();
            throw new ArgTypeFailure("TopLevel.define", expected,
                                     -1, value);
        }

        myNamespace.bind(name, fv);
    }


    private Procedure lookupProcedure(String procedureName)
        throws FusionException
    {
        SyntaxSymbol id = SyntaxSymbol.make(procedureName);

        Object proc = FusionEval.eval(myEvaluator, id, myNamespace);
        if (proc instanceof Procedure)
        {
            return (Procedure) proc;
        }

        if (proc == null)
        {
            throw new FusionException(printQuotedSymbol(procedureName) +
                                      " is not defined");
        }

        throw new FusionException(printQuotedSymbol(procedureName) +
                                  " is not a procedure: " +
                                  safeWriteToString(myEvaluator, proc));
    }

    @Override
    public Object call(String procedureName, Object... arguments)
        throws FusionException
    {
        Procedure proc = lookupProcedure(procedureName);

        for (int i = 0; i < arguments.length; i++)
        {
            Object arg = arguments[i];
            Object fv = myEvaluator.injectMaybe(arg);
            if (fv == null)
            {
                String expected =
                    "injectable Java type but received " +
                    arg.getClass().getName();
                throw new ArgTypeFailure("TopLevel.call",
                                         expected,
                                         i, arguments);
            }
            arguments[i] = fv;
        }

        return myEvaluator.callNonTail(proc, arguments);
    }
}
