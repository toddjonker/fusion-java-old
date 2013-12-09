// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.makeBool;
import com.amazon.ion.IonException;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;

/**
 *
 */
final class FusionNumber
{
    private FusionNumber() {}


    abstract static class BaseNumber
        extends FusionValue
    {
        private BaseNumber() {}

        int truncateToJavaInt()
        {
            throw new UnsupportedOperationException();
        }

        long truncateToJavaLong()
        {
            throw new UnsupportedOperationException();
        }

        /**
         * @return null if this is a Fusion null value.
         */
        BigInteger truncateToBigInteger()
        {
            throw new UnsupportedOperationException();
        }

        /**
         * @return null if this is a Fusion null value.
         */
        abstract BigDecimal toBigDecimal();

        /** First part of double-dispatch. */
        abstract BaseNumber add(BaseNumber right);
        /** Second part of double-dispatch. */
        abstract BaseNumber add(BaseInt left);

        /** First part of double-dispatch. */
        abstract BaseNumber subtract(BaseNumber right);
        /** Second part of double-dispatch. */
        abstract BaseNumber subtractFrom(BaseInt left);

        /** First part of double-dispatch. */
        abstract BaseNumber multiply(BaseNumber right);
        /** Second part of double-dispatch. */
        abstract BaseNumber multiply(BaseInt left);
    }


    //========================================================================
    // Int Representation


    abstract static class BaseInt
        extends BaseNumber
    {
        private BaseInt() {}

        @Override
        BaseNumber add(BaseNumber right)
        {
            return right.add(this);
        }

        @Override
        BaseNumber add(BaseInt left)
        {
            BigInteger leftInt = left.truncateToBigInteger();
            BigInteger rightInt = this.truncateToBigInteger();
            BigInteger result = leftInt.add(rightInt);
            return makeInt(null, result);
        }

        @Override
        BaseNumber subtract(BaseNumber right)
        {
            return right.subtractFrom(this);
        }

        @Override
        BaseNumber subtractFrom(BaseInt left)
        {
            BigInteger leftInt  = left.truncateToBigInteger();
            BigInteger rightInt = this.truncateToBigInteger();
            BigInteger result = leftInt.subtract(rightInt);
            return makeInt(null, result);
        }

        @Override
        BaseNumber multiply(BaseNumber right)
        {
            return right.multiply(this);
        }

        @Override
        BaseNumber multiply(BaseInt left)
        {
            BigInteger leftInt  = left.truncateToBigInteger();
            BigInteger rightInt = this.truncateToBigInteger();
            BigInteger result = leftInt.multiply(rightInt);
            return makeInt(null, result);
        }

    }


    private static class NullInt
        extends BaseInt
    {
        private NullInt() {}

        @Override
        boolean isAnyNull()
        {
            return true;
        }

        @Override
        BigInteger truncateToBigInteger()
        {
            return null;
        }

        @Override
        BigDecimal toBigDecimal()
        {
            return null;
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newNullInt();
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeNull(IonType.INT);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append("null.int");
        }
    }


    private static class LongInt
        extends BaseInt
    {
        private final long myContent;

        private LongInt(long content)
        {
            myContent = content;
        }

        @Override
        int truncateToJavaInt()
        {
            return (int) myContent;
        }

        @Override
        long truncateToJavaLong()
        {
            return myContent;
        }

        @Override
        BigDecimal toBigDecimal()
        {
            return BigDecimal.valueOf(myContent);
        }

        @Override
        BigInteger truncateToBigInteger()
        {
            return BigInteger.valueOf(myContent);
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newInt(myContent);
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeInt(myContent);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            // TODO WORKAROUND ION-398
            // TODO FUSION-247 Write output without the intermediate String
            out.append(Long.toString(myContent));
        }
    }


    /**
     * Truncation methods aren't implemented since we don't use this class for
     * small values.
     */
    private static class BigInt
        extends BaseInt
    {
        private final BigInteger myContent;

        /**
         * @param content must not be null.
         */
        private BigInt(BigInteger content)
        {
            myContent = content;
        }

        @Override
        BigInteger truncateToBigInteger()
        {
            return myContent;
        }

        @Override
        BigDecimal toBigDecimal()
        {
            return new BigDecimal(myContent);
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newInt(myContent);
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeInt(myContent);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            // TODO WORKAROUND ION-398
            // TODO FUSION-247 Write output without the intermediate String
            out.append(myContent.toString());
        }
    }


    private static class AnnotatedInt
        extends BaseInt
        implements Annotated
    {
        /** Not null or empty */
        final String[] myAnnotations;

        /** Not null, and not AnnotatedBool */
        final BaseInt  myValue;

        private AnnotatedInt(String[] annotations, BaseInt value)
        {
            assert annotations.length != 0;
            myAnnotations = annotations;
            myValue = value;
        }

        @Override
        int truncateToJavaInt()
        {
            return myValue.truncateToJavaInt();
        }

        @Override
        long truncateToJavaLong()
        {
            return myValue.truncateToJavaLong();
        }

        @Override
        BigDecimal toBigDecimal()
        {
            return myValue.toBigDecimal();
        }

        @Override
        BigInteger truncateToBigInteger()
        {
            return myValue.truncateToBigInteger();
        }

        @Override
        public String[] annotationsAsJavaStrings()
        {
            return myAnnotations;
        }

        @Override
        boolean isAnyNull() { return myValue.isAnyNull(); }

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
    // Decimal Representation


    abstract static class BaseDecimal
        extends BaseNumber
    {
        private BaseDecimal() {}

        @Override
        BaseNumber add(BaseNumber right)
        {
            BigDecimal leftDec  = this.toBigDecimal();
            BigDecimal rightDec = right.toBigDecimal();
            BigDecimal result = leftDec.add(rightDec);
            return makeDecimal(null, result);
        }

        @Override
        BaseNumber add(BaseInt left)
        {
            BigDecimal leftDec  = left.toBigDecimal();
            BigDecimal rightDec = this.toBigDecimal();
            BigDecimal result = leftDec.add(rightDec);
            return makeDecimal(null, result);
        }

        @Override
        BaseNumber subtract(BaseNumber right)
        {
            BigDecimal leftDec  = this.toBigDecimal();
            BigDecimal rightDec = right.toBigDecimal();
            BigDecimal result = leftDec.subtract(rightDec);
            return makeDecimal(null, result);
        }

        @Override
        BaseNumber subtractFrom(BaseInt left)
        {
            BigDecimal leftDec  = left.toBigDecimal();
            BigDecimal rightDec = this.toBigDecimal();
            BigDecimal result = leftDec.subtract(rightDec);
            return makeDecimal(null, result);
        }

        @Override
        BaseNumber multiply(BaseNumber right)
        {
            BigDecimal leftDec  = this.toBigDecimal();
            BigDecimal rightDec = right.toBigDecimal();
            BigDecimal result = leftDec.multiply(rightDec);
            return makeDecimal(null, result);
        }

        @Override
        BaseNumber multiply(BaseInt left)
        {
            BigDecimal leftDec  = left.toBigDecimal();
            BigDecimal rightDec = this.toBigDecimal();
            BigDecimal result = leftDec.multiply(rightDec);
            return makeDecimal(null, result);
        }
    }


    private static class NullDecimal
        extends BaseDecimal
    {
        private NullDecimal() {}

        @Override
        boolean isAnyNull()
        {
            return true;
        }

        @Override
        BigDecimal toBigDecimal()
        {
            return null;
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newNullDecimal();
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeNull(IonType.DECIMAL);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append("null.decimal");
        }
    }


    private static class ActualDecimal
        extends BaseDecimal
    {
        private final BigDecimal myContent;

        private ActualDecimal(BigDecimal content)
        {
            assert content != null;
            myContent = content;
        }

        @Override
        BigDecimal toBigDecimal()
        {
            return myContent;
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newDecimal(myContent);
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeDecimal(myContent);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            // TODO WORKAROUND ION-398
            // TODO FUSION-247 Write output without building an IonWriter.
            IonWriter iw = FusionValue.WRITER_BUILDER.build(out);
            iw.writeDecimal(myContent);
            iw.finish();
        }
    }


    private static class AnnotatedDecimal
        extends BaseDecimal
        implements Annotated
    {
        /** Not null or empty */
        final String[] myAnnotations;

        /** Not null, and not AnnotatedBool */
        final BaseDecimal  myValue;

        private AnnotatedDecimal(String[] annotations, BaseDecimal value)
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
        boolean isAnyNull() { return myValue.isAnyNull(); }

        @Override
        BigDecimal toBigDecimal()
        {
            return myValue.toBigDecimal();
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
    // Float Representation

    abstract static class BaseFloat
        extends FusionValue
    {
        private BaseFloat() {}

        abstract double doubleValue();
    }


    private static class NullFloat
        extends BaseFloat
    {
        private NullFloat() {}

        @Override
        double doubleValue()
        {
            throw new UnsupportedOperationException();
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
            return factory.newNullFloat();
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeNull(IonType.FLOAT);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append("null.float");
        }
    }


    private static class ActualFloat
        extends BaseFloat
    {
        private final double myContent;

        private ActualFloat(double content)
        {
            myContent = content;
        }

        @Override
        double doubleValue()
        {
            return myContent;
        }

        @Override
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            return factory.newFloat(myContent);
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeFloat(myContent);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            // TODO WORKAROUND ION-398
            // TODO FUSION-247 Write output without building an IonWriter.
            IonWriter iw = FusionValue.WRITER_BUILDER.build(out);
            iw.writeFloat(myContent);
            iw.finish();
        }
    }


    private static class AnnotatedFloat
        extends BaseFloat
        implements Annotated
    {
        /** Not null or empty */
        final String[] myAnnotations;

        /** Not null, and not AnnotatedBool */
        final BaseFloat  myValue;

        private AnnotatedFloat(String[] annotations, BaseFloat value)
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
        boolean isAnyNull() { return myValue.isAnyNull(); }

        @Override
        double doubleValue()
        {
            return myValue.doubleValue();
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
    // Int Constructors


    private static final BaseInt NULL_INT = new NullInt();

    static final BaseInt ZERO_INT = new LongInt(0);
    static final BaseInt ONE_INT  = new LongInt(1);


    private static final BigInteger MIN_LONG_VALUE =
        BigInteger.valueOf(Long.MIN_VALUE);
    private static final BigInteger MAX_LONG_VALUE =
        BigInteger.valueOf(Long.MAX_VALUE);


    /**
     * @return not null.
     */
    static BaseInt makeInt(Evaluator eval, long value)
    {
        if (value == 0) return ZERO_INT;
        if (value == 1) return ONE_INT;

        return new LongInt(value);
    }


    /**
     * @param value may be null to make {@code null.int}.
     *
     * @return not null.
     */
    static BaseInt makeInt(Evaluator eval, BigInteger value)
    {
        if (value == null) return NULL_INT;

        if (value.compareTo(MIN_LONG_VALUE) >= 0 &&
            value.compareTo(MAX_LONG_VALUE) <= 0)
        {
            long i = value.longValue();
            return new LongInt(i);
        }

        return new BigInt(value);
    }


    private static BaseInt annotate(BaseInt unannotated,
                                    String[] annotations)
    {
        assert ! (unannotated instanceof AnnotatedInt);

        if (annotations.length == 0) return unannotated;

        return new AnnotatedInt(annotations, unannotated);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     *
     * @return not null.
     */
    static BaseInt makeInt(Evaluator eval, String[] annotations, long value)
    {
        BaseInt base = makeInt(eval, value);
        return annotate(base, annotations);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null to make {@code null.int}.
     *
     * @return not null.
     */
    static BaseInt makeInt(Evaluator  eval,
                           String[]   annotations,
                           BigInteger value)
    {
        BaseInt base = makeInt(eval, value);
        return annotate(base, annotations);
    }


    /**
     * @param fusionInt must be a Fusion int.
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     *
     * @return not null.
     */
    static BaseInt unsafeIntAnnotate(Evaluator eval,
                                     Object fusionInt,
                                     String[] annotations)
    {
        BaseInt base = (BaseInt) fusionInt;
        if (base instanceof AnnotatedInt)
        {
            base = ((AnnotatedInt) base).myValue;
        }
        return annotate(base, annotations);
    }


    //========================================================================
    // Decimal Constructors


    private static final BaseDecimal NULL_DECIMAL = new NullDecimal();


    /**
     * @param value may be null to make {@code null.decimal}.
     *
     * @return not null.
     */
    static BaseDecimal makeDecimal(Evaluator eval, BigDecimal value)
    {
        return (value == null ? NULL_DECIMAL : new ActualDecimal(value));
    }


    private static BaseDecimal annotate(BaseDecimal unannotated,
                                          String[] annotations)
    {
        assert ! (unannotated instanceof AnnotatedDecimal);

        if (annotations.length == 0) return unannotated;

        return new AnnotatedDecimal(annotations, unannotated);
    }


    /**
     * @param value may be null to make {@code null.decimal}.
     *
     * @return not null.
     */
    static BaseDecimal makeDecimal(Evaluator  eval,
                                   String[]   annotations,
                                   BigDecimal value)
    {
        BaseDecimal base = makeDecimal(eval, value);
        return annotate(base, annotations);
    }


    /**
     * @param fusionDecimal must be a Fusion decimal.
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     *
     * @return not null.
     */
    static BaseDecimal unsafeDecimalAnnotate(Evaluator eval,
                                             Object fusionDecimal,
                                             String[] annotations)
    {
        BaseDecimal base = (BaseDecimal) fusionDecimal;
        if (base instanceof AnnotatedDecimal)
        {
            base = ((AnnotatedDecimal) base).myValue;
        }
        return annotate(base, annotations);
    }


    //========================================================================
    // Float Constructors


    private static final BaseFloat NULL_FLOAT = new NullFloat();


    /**
     * @return not null.
     */
    static BaseFloat makeFloat(Evaluator eval, double value)
    {
        return new ActualFloat(value);
    }


    /**
     * @param value may be null to make {@code null.float}.
     *
     * @return not null.
     */
    static BaseFloat makeFloat(Evaluator eval, Double value)
    {
        return (value == null ? NULL_FLOAT : new ActualFloat(value));
    }


    private static BaseFloat annotate(BaseFloat unannotated,
                                      String[]  annotations)
    {
        assert ! (unannotated instanceof AnnotatedFloat);

        if (annotations.length == 0) return unannotated;

        return new AnnotatedFloat(annotations, unannotated);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     *
     * @return not null.
     */
    static BaseFloat makeFloat(Evaluator eval,
                               String[]  annotations,
                               double    value)
    {
        BaseFloat base = makeFloat(eval, value);
        return annotate(base, annotations);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null to make {@code null.float}.
     *
     * @return not null.
     */
    static BaseFloat makeFloat(Evaluator eval,
                               String[]  annotations,
                               Double    value)
    {
        BaseFloat base = makeFloat(eval, value);
        return annotate(base, annotations);
    }


    /**
     * @param fusionFloat must be a Fusion float.
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     *
     * @return not null.
     */
    static BaseFloat unsafeFloatAnnotate(Evaluator eval,
                                         Object    fusionFloat,
                                         String[]  annotations)
    {
        BaseFloat base = (BaseFloat) fusionFloat;
        if (base instanceof AnnotatedFloat)
        {
            base = ((AnnotatedFloat) base).myValue;
        }
        return annotate(base, annotations);
    }


    //========================================================================
    // Predicates


    public static boolean isInt(TopLevel top, Object value)
        throws FusionException
    {
        return (value instanceof BaseInt);
    }

    static boolean isInt(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof BaseInt);
    }


    public static boolean isDecimal(TopLevel top, Object value)
        throws FusionException
    {
        return (value instanceof BaseDecimal);
    }

    static boolean isDecimal(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof BaseDecimal);
    }


    static boolean isIntOrDecimal(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof BaseInt || value instanceof BaseDecimal);
    }


    public static boolean isFloat(TopLevel top, Object value)
        throws FusionException
    {
        return (value instanceof BaseFloat);
    }

    static boolean isFloat(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof BaseFloat);
    }


    //========================================================================
    // Conversions


    /**
     * @param fusionInt must be a non-null int.
     */
    static int unsafeTruncateIntToJavaInt(Evaluator eval, Object fusionInt)
        throws FusionException
    {
        return ((BaseInt) fusionInt).truncateToJavaInt();
    }


    /**
     * @param fusionInt must be a non-null int.
     */
    static long unsafeTruncateIntToJavaLong(Evaluator eval, Object fusionInt)
        throws FusionException
    {
        return ((BaseInt) fusionInt).truncateToJavaLong();
    }


    static BigInteger unsafeIntToJavaBigInteger(TopLevel top,
                                                Object   fusionInt)
        throws FusionException
    {
        return ((BaseInt) fusionInt).truncateToBigInteger();
    }

    static BigInteger unsafeIntToJavaBigInteger(Evaluator eval,
                                                Object    fusionInt)
        throws FusionException
    {
        return ((BaseInt) fusionInt).truncateToBigInteger();
    }


    static double unsafeFloatToDouble(TopLevel top, Object fusionFloat)
        throws FusionException
    {
        return ((BaseFloat) fusionFloat).doubleValue();
    }

    static double unsafeFloatToDouble(Evaluator eval, Object fusionFloat)
        throws FusionException
    {
        return ((BaseFloat) fusionFloat).doubleValue();
    }


    static BigDecimal unsafeNumberToBigDecimal(TopLevel top,
                                               Object   fusionNumber)
        throws FusionException
    {
        return ((BaseNumber) fusionNumber).toBigDecimal();
    }

    static BigDecimal unsafeNumberToBigDecimal(Evaluator eval,
                                               Object    fusionNumber)
        throws FusionException
    {
        return ((BaseNumber) fusionNumber).toBigDecimal();
    }


    //========================================================================
    // Int Procedure Helpers


    /**
     * Checks that an argument is a Fusion int and that its value fits safely
     * into Java's {@code int} type.
     */
    static int checkIntArgToJavaInt(Evaluator eval,
                                    Procedure who,
                                    int       argNum,
                                    Object... args)
        throws FusionException, ArgTypeFailure
    {
        Object arg = args[argNum];
        if (arg instanceof LongInt)
        {
            long i = ((LongInt) arg).myContent;
            if (Integer.MIN_VALUE <= i && i <= Integer.MAX_VALUE)
            {
                return (int) i;
            }
        }

        throw who.argFailure("32-bit int", argNum, args);
    }


    /**
     * Checks that an argument is a Fusion int and that its value fits safely
     * into Java's {@code long} type.
     */
    static long checkIntArgToJavaLong(Evaluator eval,
                                      Procedure who,
                                      int       argNum,
                                      Object... args)
        throws FusionException, ArgTypeFailure
    {
        Object arg = args[argNum];
        if (arg instanceof LongInt)
        {
            long i = ((LongInt) arg).myContent;
            if (Long.MIN_VALUE <= i && i <= Long.MAX_VALUE)
            {
                return (int) i;
            }
        }

        throw who.argFailure("64-bit int", argNum, args);
    }


    /**
     * @param expectation must not be null.
     * @return may be null
     */
    static BigInteger checkIntArg(Evaluator eval,
                                  Procedure who,
                                  String    expectation,
                                  int       argNum,
                                  Object... args)
        throws FusionException, ArgTypeFailure
    {
        Object arg = args[argNum];
        if (arg instanceof BaseInt)
        {
            return ((BaseInt) arg).truncateToBigInteger();
        }

        throw who.argFailure(expectation, argNum, args);
    }


    /**
     * @return may be null.
     */
    static BigInteger checkNullableIntArg(Evaluator eval,
                                          Procedure who,
                                          int argNum,
                                          Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "nullable int";
        return checkIntArg(eval, who, expectation, argNum, args);
    }


    /**
     * @return not null
     */
    static BigInteger checkRequiredIntArg(Evaluator eval,
                                          Procedure who,
                                          int       argNum,
                                          Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "non-null int";
        BigInteger result = checkIntArg(eval, who, expectation, argNum, args);
        if (result == null)
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return result;
    }


    //========================================================================
    // Decimal Procedure Helpers


    /**
     * @param expectation must not be null.
     * @return may be null
     */
    static BigDecimal checkDecimalArg(Evaluator eval,
                                      Procedure who,
                                      String    expectation,
                                      int       argNum,
                                      Object... args)
        throws FusionException, ArgTypeFailure
    {
        Object arg = args[argNum];
        if (arg instanceof BaseDecimal)
        {
            return ((BaseDecimal) arg).toBigDecimal();
        }

        throw who.argFailure(expectation, argNum, args);
    }


    /**
     * @return not null
     */
    static BigDecimal checkRequiredDecimalArg(Evaluator eval,
                                              Procedure who,
                                              int       argNum,
                                              Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "non-null decimal";
        BigDecimal result =
            checkDecimalArg(eval, who, expectation, argNum, args);
        if (result == null)
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return result;
    }


    //========================================================================
    // Float Procedure Helpers


    /**
     * @param expectation must not be null.
     * @return may be null
     */
    static Double checkFloatArg(Evaluator eval,
                                Procedure who,
                                String    expectation,
                                int       argNum,
                                Object... args)
        throws FusionException, ArgTypeFailure
    {
        Object arg = args[argNum];
        if (arg instanceof BaseFloat)
        {
            return ((BaseFloat) arg).doubleValue();
        }

        throw who.argFailure(expectation, argNum, args);
    }


    static Double checkNullableFloatArg(Evaluator eval,
                                        Procedure who,
                                        int       argNum,
                                        Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "nullable float";
        return checkFloatArg(eval, who, expectation, argNum, args);
    }


    /**
     * @return not null
     */
    static double checkRequiredFloatArg(Evaluator eval,
                                        Procedure who,
                                        int       argNum,
                                        Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "non-null float";

        // TODO avoid wrap/unwrap
        Double result = checkFloatArg(eval, who, expectation, argNum, args);
        if (result == null)
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return result;
    }


    //========================================================================
    // Procedures


    static final class IsIntProc
        extends Procedure1
    {
        IsIntProc()
        {
            //    "                                                                               |
            super("Determines whether a `value` is of type `int`, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean r = isInt(eval, arg);
            return makeBool(eval, r);
        }
    }


    static final class IsDecimalProc
        extends Procedure1
    {
        IsDecimalProc()
        {
            //    "                                                                               |
            super("Determines whether a `value` is of type `decimal`, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean r = isDecimal(eval, arg);
            return makeBool(eval, r);
        }
    }


    static final class IsFloatProc
        extends Procedure1
    {
        IsFloatProc()
        {
            //    "                                                                               |
            super("Determines whether a `value` is of type `float`, returning `true` or `false`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean r = isFloat(eval, arg);
            return makeBool(eval, r);
        }
    }
}
