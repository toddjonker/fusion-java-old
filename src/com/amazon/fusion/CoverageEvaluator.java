// Copyright (c) 2014-2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 *
 */
final class CoverageEvaluator
    extends Evaluator
{
    private final _Private_CoverageCollector myCollector;

    CoverageEvaluator(GlobalState globalState,
                      _Private_CoverageCollector collector)
    {
        super(globalState);
        myCollector = collector;
    }

    private CoverageEvaluator(CoverageEvaluator outer,
                              Object key,
                              Object mark)
    {
        super(outer, key, mark);
        myCollector = outer.myCollector;
    }

    private CoverageEvaluator(CoverageEvaluator outer,
                              Object[] keys,
                              Object[] marks)
    {
        super(outer, keys, marks);
        myCollector = outer.myCollector;
    }


    @Override
    Evaluator markedContinuation(Object key, Object mark)
    {
        return new CoverageEvaluator(this, key, mark);
    }

    @Override
    Evaluator markedContinuation(Object[] keys, Object[] marks)
    {
        return new CoverageEvaluator(this, keys, marks);
    }


    @Override
    Compiler makeCompiler()
    {
        return new CoverageCompiler();
    }


    /**
     * An extended {@link Compiler} that generates instrumented code.
     */
    private final class CoverageCompiler
        extends Compiler
    {
        CoverageCompiler()
        {
            super(CoverageEvaluator.this);
        }

        @Override
        CompiledForm compileExpression(Environment env, SyntaxValue source)
            throws FusionException
        {
            CompiledForm form = super.compileExpression(env, source);

            SourceLocation loc = source.getLocation();
            if (loc != null)
            {
                if (myCollector.coverableLocation(loc))
                {
                    form = new CoverageCompiledForm(loc, form);
                }
            }

            return form;
        }
    }


    /**
     * Decorator that notifies the {@link _Private_CoverageCollector} when a
     * form has been evaluated.
     */
    private static final class CoverageCompiledForm
        implements CompiledForm
    {
        private final SourceLocation myLocation;
        private final CompiledForm   myForm;

        CoverageCompiledForm(SourceLocation location,
                             CompiledForm   decorated)
        {
            myLocation  = location;
            myForm      = decorated;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            _Private_CoverageCollector collector =
                ((CoverageEvaluator) eval).myCollector;
            collector.coverLocation(myLocation);

            return myForm.doEval(eval, store);
        }
    }
}
