// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import com.amazon.ion.IonException;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import java.io.IOException;

/**
 * Implementation of {@code null.null}.
 */
final class FusionNull
{
    /** Not for application use. */
    private FusionNull() {}


    static class NullNull
        extends FusionValue
    {
        private NullNull() {}

        String[] annotationsAsJavaStrings()
        {
            return EMPTY_STRING_ARRAY;
        }

        @Override
        boolean isAnyNull()
        {
            return true;
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newNull();
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeNull();
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append("null");
        }
    }


    private static final class AnnotatedNullNull
        extends NullNull
        implements Annotated
    {
        /** Not null or empty */
        final String[] myAnnotations;

        AnnotatedNullNull(String[] annotations)
        {
            assert annotations.length != 0;
            myAnnotations = annotations;
        }

        @Override
        public String[] annotationsAsJavaStrings()
        {
            return myAnnotations;
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            IonValue iv = factory.newNull();
            iv.setTypeAnnotations(myAnnotations);
            return iv;
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.setTypeAnnotations(myAnnotations);
            out.writeNull();
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            writeAnnotations(out, myAnnotations);
            out.append("null");
        }
    }


    //========================================================================
    // Constructors


    private static final NullNull NULL_NULL = new NullNull();


    static Object makeNullNull(Evaluator eval)
    {
        return NULL_NULL;
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     *
     * @return not null.
     */
    static Object makeNullNull(Evaluator eval, String[] annotations)
    {
        if (annotations.length == 0)
        {
            return NULL_NULL;
        }

        return new AnnotatedNullNull(annotations);
    }


    //========================================================================
    // Predicates


    static boolean isNullNull(Evaluator eval, Object value)
    {
        return (value instanceof NullNull);
    }


    //========================================================================
    // Procedures


    static final class IsNullNullProc
        extends Procedure1
    {
        IsNullNullProc()
        {
            //    "                                                                               |
            super("Returns `true` when `value` is `null.null`, `false` otherwise.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean isNull = isNullNull(eval, arg);
            return eval.newBool(isNull);
        }
    }
}
