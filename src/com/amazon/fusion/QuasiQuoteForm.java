// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionList.immutableList;
import static com.amazon.fusion.FusionSexp.immutableSexp;

final class QuasiQuoteForm
    extends QuasiBaseForm
{
    public QuasiQuoteForm(Object qqIdentifier,
                          Object uqIdentifier)
    {
        super(qqIdentifier, uqIdentifier);
    }


    //========================================================================


    @Override
    CompiledConstant constant(Evaluator eval, SyntaxValue quotedStx)
        throws FusionException
    {
        Object datum = quotedStx.syntaxToDatum(eval);
        return new CompiledConstant(datum);
    }


    @Override
    CompiledForm unquote(Evaluator    eval,
                         SyntaxValue  unquotedStx,
                         CompiledForm unquotedForm)
        throws FusionException
    {
        return unquotedForm;
    }


    @Override
    CompiledForm quasiSexp(Evaluator      eval,
                           SyntaxSexp     originalStx,
                           CompiledForm[] children)
        throws FusionException
    {
        String[] annotations = originalStx.annotationsAsJavaStrings();
        return new CompiledQuasiQuoteSexp(annotations, children);
    }


    @Override
    CompiledForm quasiList(Evaluator      eval,
                           SyntaxList     originalStx,
                           CompiledForm[] children)
        throws FusionException
    {
        String[] annotations = originalStx.annotationsAsJavaStrings();
        return new CompiledQuasiQuoteList(annotations, children);
    }


    //========================================================================


    private static final class CompiledQuasiQuoteSexp
        implements CompiledForm
    {
        private final String[]       myAnnotations;
        private final CompiledForm[] myChildForms;

        CompiledQuasiQuoteSexp(String[]       annotations,
                               CompiledForm[] childForms)
        {
            assert childForms.length != 0;
            myAnnotations = annotations;
            myChildForms  = childForms;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            int size = myChildForms.length;
            Object[] children = new Object[size];
            for (int i = 0; i < size; i++)
            {
                children[i] = eval.eval(store, myChildForms[i]);
            }
            return immutableSexp(eval, myAnnotations, children);
        }
    }


    private static final class CompiledQuasiQuoteList
        implements CompiledForm
    {
        private final String[]       myAnnotations;
        private final CompiledForm[] myChildForms;

        CompiledQuasiQuoteList(String[]       annotations,
                               CompiledForm[] childForms)
        {
            myAnnotations = annotations;
            myChildForms  = childForms;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            int size = myChildForms.length;
            Object[] children = new Object[size];
            for (int i = 0; i < size; i++)
            {
                children[i] = eval.eval(store, myChildForms[i]);
            }
            return immutableList(eval, myAnnotations, children);
        }
    }
}
