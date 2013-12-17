// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.falseBool;
import static com.amazon.fusion.FusionBool.makeBool;
import com.amazon.fusion.FusionBool.BaseBool;
import com.amazon.fusion.FusionLob.BaseLob;
import com.amazon.ion.IonException;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import java.io.IOException;

/**
 *
 */
final class FusionBlob
{
    private FusionBlob() {}


    abstract static class BaseBlob
        extends BaseLob
    {
        private BaseBlob() {}

        @Override
        BaseBlob annotate(Evaluator eval, String[] annotations)
        {
            return FusionBlob.annotate(this, annotations);
        }
    }


    private static class NullBlob
        extends BaseBlob
    {
        private NullBlob() {}

        @Override
        boolean isAnyNull()
        {
            return true;
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            boolean b = (right instanceof BaseBlob
                         && ((BaseBlob) right).isAnyNull());
            return makeBool(eval, b);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return isAnyNull(eval, right);
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newNullBlob();
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeNull(IonType.BLOB);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append("null.blob");
        }
    }


    private static class ActualBlob
        extends BaseBlob
    {
        private final byte[] myContent;

        private ActualBlob(byte[] content)
        {
            assert content != null;
            myContent = content;
        }

        @Override
        byte[] bytesNoCopy()
        {
            return myContent;
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
        {
            if (right instanceof BaseBlob)
            {
                return actualLobEquals(eval, myContent, right);
            }
            return falseBool(eval);
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newBlob(myContent);
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeBlob(myContent);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            // TODO WORKAROUND ION-398
            // TODO FUSION-247 Write output without building an IonWriter.
            IonWriter iw = WRITER_BUILDER.build(out);
            iw.writeBlob(myContent);
            iw.finish();
        }
    }


    private static class AnnotatedBlob
        extends BaseBlob
        implements Annotated
    {
        /** Not null or empty */
        final String[] myAnnotations;

        /** Not null, and not AnnotatedBool */
        final BaseBlob  myValue;

        private AnnotatedBlob(String[] annotations, BaseBlob value)
        {
            assert annotations.length != 0;
            myAnnotations = annotations;
            myValue = value;
        }

        @Override
        public String[] annotationsAsJavaStrings()
        {
            return myAnnotations;
        }

        @Override
        BaseBlob annotate(Evaluator eval, String[] annotations)
        {
            return FusionBlob.annotate(myValue, annotations);
        }

        @Override
        boolean isAnyNull()
        {
            return myValue.isAnyNull();
        }

        @Override
        byte[] bytesNoCopy()
        {
            return myValue.bytesNoCopy();
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return myValue.tightEquals(eval, right);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return myValue.looseEquals(eval, right);
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            IonValue iv = myValue.copyToIonValue(factory,
                                                 throwOnConversionFailure);
            iv.setTypeAnnotations(myAnnotations);
            return iv;
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.setTypeAnnotations(myAnnotations);
            myValue.ionize(eval, out);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            writeAnnotations(out, myAnnotations);
            myValue.write(eval, out);
        }
    }


    //========================================================================
    // Constructors


    private static final BaseBlob NULL_BLOB = new NullBlob();


    /**
     * @param value may be null to make {@code null.blob}.
     * This method assumes ownership of the array and it must not be modified
     * later.
     *
     * @return not null.
     */
    static BaseBlob makeBlob(Evaluator eval, byte[] value)
    {
        return (value == null ? NULL_BLOB : new ActualBlob(value));
    }


    private static BaseBlob annotate(BaseBlob unannotated,
                                     String[] annotations)
    {
        assert ! (unannotated instanceof AnnotatedBlob);

        if (annotations.length == 0) return unannotated;

        return new AnnotatedBlob(annotations, unannotated);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null to make {@code null.blob}.
     * This method assumes ownership of the array and it must not be modified
     * later.
     *
     * @return not null.
     */
    static BaseBlob makeBlob(Evaluator eval,
                             String[]  annotations,
                             byte[]    value)
    {
        BaseBlob base = makeBlob(eval, value);
        return annotate(base, annotations);
    }


    /**
     * @param fusionBlob must be a Fusion blob.
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     *
     * @return not null.
     */
    static BaseBlob unsafeBlobAnnotate(Evaluator eval,
                                       Object fusionBlob,
                                       String[] annotations)
    {
        BaseBlob base = (BaseBlob) fusionBlob;
        return base.annotate(eval, annotations);
    }


    //========================================================================
    // Predicates


    public static boolean isBlob(TopLevel top, Object value)
        throws FusionException
    {
        return (value instanceof BaseBlob);
    }

    static boolean isBlob(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof BaseBlob);
    }


    //========================================================================
    // Conversions




    //========================================================================
    // Procedure Helpers




    //========================================================================
    // Procedures


    static final class IsBlobProc
        extends Procedure1
    {
        IsBlobProc()
        {
            //    "                                                                               |
            super("Determines whether a `value` is of type `blob`, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean r = isBlob(eval, arg);
            return makeBool(eval, r);
        }
    }
}
