// Copyright (c) 2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIo.safeWrite;
import static com.amazon.fusion.FusionVoid.voidValue;
import static com.amazon.fusion.LetValuesForm.compilePlainLet;
import com.amazon.fusion.LambdaForm.CompiledLambdaBase;
import com.amazon.fusion.LambdaForm.CompiledLambdaExact;

/**
 * "Registers" used during compilation.
 */
class Compiler
{
    private final Evaluator myEval;

    Compiler(Evaluator eval)
    {
        myEval = eval;
    }


    final Evaluator getEvaluator()
    {
        return myEval;
    }

    final GlobalState getGlobalState()
    {
        return myEval.getGlobalState();
    }

    final ModuleInstance getKernel()
    {
        return myEval.findKernel();
    }


    /**
     * Compiles a single fully-expanded core syntax form.
     */
    CompiledForm compileExpression(Environment env, SyntaxValue source)
        throws FusionException
    {
        return source.doCompile(this, env);
    }


    /**
     * Compiles a sequence of individual expressions.
     *
     * @return not null, but perhaps {@link CompiledForm#EMPTY_ARRAY}.
     */
    CompiledForm[] compileExpressions(Environment env, SyntaxSequence source,
                                      int from, int to)
        throws FusionException
    {
        int size = to - from;

        if (size == 0) return CompiledForm.EMPTY_ARRAY;

        CompiledForm[] forms = new CompiledForm[size];
        for (int i = from; i < to; i++)
        {
            SyntaxValue form = source.get(myEval, i);
            forms[i - from] = compileExpression(env, form);
        }

        return forms;
    }


    /**
     * Compiles a sequence of individual expressions.
     *
     * @return not null, but perhaps {@link CompiledForm#EMPTY_ARRAY}.
     */
    CompiledForm[] compileExpressions(Environment env, SyntaxSequence source,
                                      int from)
        throws FusionException
    {
        return compileExpressions(env, source, from, source.size());
    }


    CompiledForm compile(Environment env, SyntacticForm form, SyntaxSexp stx)
        throws FusionException
    {
        return form.compile(this, env, stx);
    }


    /**
     * Compiles a sequence of expressions as if in a {@code begin} expression.
     */
    final CompiledForm compileBegin(Environment env, SyntaxSexp stx,
                                    int from, int to)
        throws FusionException
    {
        int size = to - from;

        if (size == 0) return new CompiledConstant(voidValue(myEval));

        if (size == 1) return compileExpression(env, stx.get(myEval, from));

        CompiledForm[] subforms = compileExpressions(env, stx, from, to);
        return new CompiledBegin(subforms);
    }

    CompiledForm compileBegin(Environment env, SyntaxSexp stx, int from)
        throws FusionException
    {
        return compileBegin(env, stx, from, stx.size());
    }


    CompiledForm compileProcedureApplication(Environment env,
                                             SyntaxSexp stx,
                                             SyntaxValue procExpr)
        throws FusionException
    {
        CompiledForm procForm = compileExpression(env, procExpr);
        CompiledForm[] argForms = compileExpressions(env, stx, 1);

        if (procForm instanceof CompiledLambdaExact)
        {
            CompiledLambdaBase lambda = (CompiledLambdaBase) procForm;
            if (lambda.myArgNames.length != argForms.length)
            {
                String message =
                    "procedure expects " + lambda.myArgNames.length +
                    " arguments but application has " + argForms.length +
                    " expressions";
                 throw new SyntaxException("procedure application", message,
                                           stx);
            }

            return compilePlainLet(argForms, lambda.myBody);
        }

        return new CompiledPlainApp(stx.getLocation(), procForm, argForms);
    }


    //========================================================================


    private static final class CompiledBegin
        implements CompiledForm
    {
        final CompiledForm[] myBody;

        CompiledBegin(CompiledForm[] body)
        {
            myBody = body;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            final int last = myBody.length - 1;
            for (int i = 0; i < last; i++)
            {
                CompiledForm form = myBody[i];
                eval.eval(store, form);
            }

            CompiledForm form = myBody[last];
            return eval.bounceTailForm(store, form);
        }
    }


    private static final class CompiledPlainApp
        implements CompiledForm
    {
        private final SourceLocation myLocation;
        private final CompiledForm   myProcForm;
        private final CompiledForm[] myArgForms;

        CompiledPlainApp(SourceLocation location,
                         CompiledForm   procForm,
                         CompiledForm[] argForms)
        {
            myLocation = location;
            myProcForm = procForm;
            myArgForms = argForms;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Object proc = eval.eval(store, myProcForm, myLocation);

            int argCount = myArgForms.length;

            Object[] args;
            if (argCount == 0)
            {
                args = FusionUtils.EMPTY_OBJECT_ARRAY;
            }
            else
            {
                args = new Object[argCount];
                for (int i = 0; i < argCount; i++)
                {
                    args[i] = eval.eval(store, myArgForms[i], myLocation);
                }
            }

            Procedure p;
            try
            {
                p = (Procedure) proc;
            }
            catch (ClassCastException e)
            {
                StringBuilder b = new StringBuilder();
                b.append("Application expected procedure, given: ");
                safeWrite(eval, b, proc);
                if (args.length == 0)
                {
                    b.append("\nNo arguments were provided.");
                }
                else
                {
                    b.append("\nArguments were: ");
                    for (int i = 0; i < args.length; i++)
                    {
                        b.append("\n  ");
                        safeWrite(eval, b, args[i]);
                    }
                }

                FusionException fe = new FusionException(b.toString());
                fe.addContext(myLocation);
                throw fe;
            }

            return eval.bounceTailCall(myLocation, p, args);
        }
    }
}
