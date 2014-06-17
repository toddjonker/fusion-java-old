// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.falseBool;
import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionBool.trueBool;
import static com.amazon.fusion.FusionIo.safeWriteToString;
import static com.amazon.fusion.SimpleSyntaxValue.makeSyntax;
import static java.math.RoundingMode.CEILING;
import static java.math.RoundingMode.FLOOR;
import com.amazon.fusion.FusionBool.BaseBool;
import com.amazon.ion.Decimal;
import com.amazon.ion.IonException;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

/**
 *
 */
final class FusionNumber
{
    private FusionNumber() {}


    abstract static class BaseNumber
        extends BaseValue
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

        /**
         * Compare value strictly, annotations cannot match.
         * Instance identity has already been performed by the caller.
         */
        BaseBool isSame(Evaluator eval, Object right)
            throws FusionException
        {
            return falseBool(eval);
        }

        /**
         * Second part of double-dispatch.
         * @param left is not a null value
         */
        BaseBool looseEquals2(Evaluator eval, BaseInt left)
            throws FusionException
        {
            return falseBool(eval);
        }

        BaseBool looseEquals2(Evaluator eval, ActualFloat left)
            throws FusionException
        {
            return falseBool(eval);
        }

        @Override
        SyntaxValue datumToSyntaxMaybe(Evaluator eval, SourceLocation loc)
        {
            return makeSyntax(eval, loc, this);
        }
    }


    private static BaseBool looseEqualsAsDecimal(Evaluator eval,
                                                 BaseNumber left,
                                                 BaseNumber right)
        throws FusionException
    {
        BigDecimal leftDec  = left.toBigDecimal();
        BigDecimal rightDec = right.toBigDecimal();
        boolean result = leftDec.compareTo(rightDec) == 0;
        return makeBool(eval, result);
    }


    //========================================================================
    // Int Representation


    abstract static class BaseInt
        extends BaseNumber
    {
        private BaseInt() {}

        @Override
        BaseInt annotate(Evaluator eval, String[] annotations)
        {
            return FusionNumber.annotate(this, annotations);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseNumber)
            {
                return ((BaseNumber) right).looseEquals2(eval, this);
            }

            return falseBool(eval);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, BaseInt left)
            throws FusionException
        {
            // Both left and right (this) are ints, so "truncation" doesn't
            // affect the value.  Also, neither are null.int due to override
            // of this method by NullInt.
            BigInteger leftInt  = left.truncateToBigInteger();
            BigInteger rightInt = this.truncateToBigInteger();
            boolean result = leftInt.equals(rightInt);
            return makeBool(eval, result);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, ActualFloat left)
            throws FusionException
        {
            return looseEqualsAsDecimal(eval, left, this);
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseInt)
            {
                return ((BaseInt) right).looseEquals2(eval, this);
            }

            return falseBool(eval);
        }

        @Override
        BaseNumber add(BaseNumber right)
        {
            return right.add(this);
        }

        @Override
        BaseNumber add(BaseInt left)
        {
            BigInteger leftInt  = left.truncateToBigInteger();
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
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            boolean b = (right instanceof BaseInt
                         && ((BaseInt) right).isAnyNull());
            return makeBool(eval, b);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return isAnyNull(eval, right);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, BaseInt left)
            throws FusionException
        {
            // left is not null, but right (this) is null.
            return falseBool(eval);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, ActualFloat left)
            throws FusionException
        {
            // left is not null, but right (this) is null.
            return falseBool(eval);
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
        BaseBool isSame(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof LongInt)
            {
                if (myContent == ((LongInt) right).myContent)
                {
                    return trueBool(eval);
                }
            }
            return falseBool(eval);
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
        BaseBool isSame(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BigInt)
            {
                BigInteger rightInt = ((BigInt) right).myContent;
                if (myContent.equals(rightInt))
                {
                    return trueBool(eval);
                }
            }
            return falseBool(eval);
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
        public String[] annotationsAsJavaStrings()
        {
            return myAnnotations;
        }

        @Override
        BaseInt annotate(Evaluator eval, String[] annotations)
        {
            return myValue.annotate(eval, annotations);
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
        boolean isAnyNull() { return myValue.isAnyNull(); }

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
        BaseBool looseEquals2(Evaluator eval, ActualFloat left)
            throws FusionException
        {
            return myValue.looseEquals2(eval, left);
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
    // Decimal Representation


    abstract static class BaseDecimal
        extends BaseNumber
    {
        private BaseDecimal() {}

        @Override
        BaseDecimal annotate(Evaluator eval, String[] annotations)
        {
            return FusionNumber.annotate(this, annotations);
        }

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
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            boolean b = (right instanceof BaseDecimal
                         && ((BaseDecimal) right).isAnyNull());
            return makeBool(eval, b);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return isAnyNull(eval, right);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, BaseInt left)
            throws FusionException
        {
            // left is not null, but right (this) is null.
            return falseBool(eval);
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
        BaseBool isSame(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof ActualDecimal)
            {
                BigDecimal rightDec = ((ActualDecimal) right).myContent;
                if (Decimal.equals(myContent, rightDec))
                {
                    return trueBool(eval);
                }
            }
            return falseBool(eval);
        }

        @Override
        BaseBool strictEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseDecimal)
            {
                BigDecimal rightDec = ((BaseDecimal) right).toBigDecimal();
                if (rightDec != null && Decimal.equals(myContent, rightDec))
                {
                    return trueBool(eval);
                }
            }
            return falseBool(eval);
        }

        private BaseBool tightEquals(Evaluator eval, BigDecimal rightDec)
        {
            if (rightDec != null)
            {
                if (myContent.compareTo(rightDec) == 0)
                {
                    return trueBool(eval);
                }
            }
            return falseBool(eval);
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseDecimal)
            {
                BigDecimal rightDec = ((BaseDecimal) right).toBigDecimal();
                return tightEquals(eval, rightDec);
            }
            return falseBool(eval);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseNumber)
            {
                BigDecimal rightDec = ((BaseNumber) right).toBigDecimal();
                return tightEquals(eval, rightDec);
            }

            return falseBool(eval);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, BaseInt left)
            throws FusionException
        {
            BigDecimal leftDec  = left.toBigDecimal();
            boolean result = leftDec.compareTo(myContent) == 0;
            return makeBool(eval, result);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, ActualFloat left)
            throws FusionException
        {
            BigDecimal leftDec  = left.toBigDecimal();
            boolean result = leftDec.compareTo(myContent) == 0;
            return makeBool(eval, result);
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
            IonTextUtils.printDecimal(out, myContent);
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
        BaseDecimal annotate(Evaluator eval, String[] annotations)
        {
            return myValue.annotate(eval, annotations);
        }

        @Override
        boolean isAnyNull() { return myValue.isAnyNull(); }

        @Override
        BigDecimal toBigDecimal()
        {
            return myValue.toBigDecimal();
        }

        @Override
        BaseBool strictEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return myValue.strictEquals(eval, right);
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
        BaseBool looseEquals2(Evaluator eval, BaseInt right)
            throws FusionException
        {
            return myValue.looseEquals(eval, right);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, ActualFloat left)
            throws FusionException
        {
            return myValue.looseEquals2(eval, left);
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
        extends BaseNumber
    {
        private BaseFloat() {}

        /**
         * This must not be {@code null.float}.
         */
        abstract double primitiveDoubleValue();

        abstract Double objectDoubleValue();

        abstract BaseBool tightEquals2(Evaluator eval, ActualFloat left)
            throws FusionException;

        @Override
        BaseFloat annotate(Evaluator eval, String[] annotations)
        {
            return FusionNumber.annotate(this, annotations);
        }
    }


    private static class NullFloat
        extends BaseFloat
    {
        private NullFloat() {}

        @Override
        double primitiveDoubleValue()
        {
            throw new UnsupportedOperationException();
        }

        @Override
        Double objectDoubleValue()
        {
            return null;
        }

        @Override
        BigDecimal toBigDecimal()
        {
            return null;
        }

        @Override
        boolean isAnyNull()
        {
            return true;
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            boolean b = (right instanceof BaseFloat
                         && ((BaseFloat) right).isAnyNull());
            return makeBool(eval, b);
        }

        @Override
        BaseBool tightEquals2(Evaluator eval, ActualFloat right)
            throws FusionException
        {
            return falseBool(eval);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return isAnyNull(eval, right);
        }

        @Override
        BaseNumber add(BaseNumber right)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber add(BaseInt left)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber subtract(BaseNumber right)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber subtractFrom(BaseInt left)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber multiply(BaseNumber right)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber multiply(BaseInt left)
        {
            throw new UnsupportedOperationException();
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
        double primitiveDoubleValue()
        {
            return myContent;
        }

        @Override
        Double objectDoubleValue()
        {
            return myContent;
        }

        @Override
        BigDecimal toBigDecimal()
        {
            return new BigDecimal(myContent);
        }

        @Override
        BaseBool isSame(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof ActualFloat)
            {
                double rightDouble = ((ActualFloat) right).myContent;
                if (Double.compare(myContent, rightDouble) == 0)
                {
                    return trueBool(eval);
                }
            }
            return falseBool(eval);
        }

        @Override
        BaseBool strictEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseFloat)
            {
                Double rightDouble = ((BaseFloat) right).objectDoubleValue();
                if (rightDouble != null
                    && Double.compare(myContent, rightDouble) == 0)
                {
                    return trueBool(eval);
                }
            }
            return falseBool(eval);
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseFloat)
            {
                return ((BaseFloat) right).tightEquals2(eval, this);
            }
            return falseBool(eval);
        }

        @Override
        BaseBool tightEquals2(Evaluator eval, ActualFloat left)
            throws FusionException
        {
            return makeBool(eval, left.myContent == this.myContent);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseNumber)
            {
                return ((BaseNumber) right).looseEquals2(eval, this);
            }

            return falseBool(eval);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, BaseInt left)
            throws FusionException
        {
            return looseEqualsAsDecimal(eval, left, this);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, ActualFloat left)
            throws FusionException
        {
            return makeBool(eval, myContent == left.myContent);
        }

        @Override
        BaseNumber add(BaseNumber right)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber add(BaseInt left)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber subtract(BaseNumber right)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber subtractFrom(BaseInt left)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber multiply(BaseNumber right)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber multiply(BaseInt left)
        {
            throw new UnsupportedOperationException();
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
            IonTextUtils.printFloat(out, myContent);
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
        BaseFloat annotate(Evaluator eval, String[] annotations)
        {
            return FusionNumber.annotate(myValue, annotations);
        }

        @Override
        boolean isAnyNull() { return myValue.isAnyNull(); }

        @Override
        double primitiveDoubleValue()
        {
            return myValue.primitiveDoubleValue();
        }

        @Override
        Double objectDoubleValue()
        {
            return myValue.objectDoubleValue();
        }

        @Override
        BigDecimal toBigDecimal()
        {
            return myValue.toBigDecimal();
        }


        @Override
        BaseBool strictEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return myValue.strictEquals(eval, right);
        }

        @Override
        BaseBool tightEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return myValue.tightEquals(eval, right);
        }

        @Override
        BaseBool tightEquals2(Evaluator eval, ActualFloat left)
            throws FusionException
        {
            return myValue.tightEquals2(eval, left);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            return myValue.looseEquals(eval, right);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, BaseInt left)
            throws FusionException
        {
            return myValue.looseEquals2(eval, left);
        }

        @Override
        BaseBool looseEquals2(Evaluator eval, ActualFloat left)
            throws FusionException
        {
            return myValue.looseEquals2(eval, left);
        }

        @Override
        BaseNumber add(BaseNumber right)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber add(BaseInt left)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber subtract(BaseNumber right)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber subtractFrom(BaseInt left)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber multiply(BaseNumber right)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        BaseNumber multiply(BaseInt left)
        {
            throw new UnsupportedOperationException();
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


    // We may want to optimize this further
    static BaseInt makeInt(Evaluator eval, int value)
    {
        return makeInt(eval, (long) value);
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
        return base.annotate(eval, annotations);
    }


    //========================================================================
    // Decimal Constructors


    private static final BaseDecimal NULL_DECIMAL = new NullDecimal();


    /**
     * @param value may be null to make {@code null.decimal}. An instance of
     * {@link Decimal} should be used to create a negative zero value.
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
     * @param value may be null to make {@code null.decimal}. An instance of
     * {@link Decimal} should be used to create a negative zero value.
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
        return base.annotate(eval, annotations);
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
        return base.annotate(eval, annotations);
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
        return ((BaseFloat) fusionFloat).primitiveDoubleValue();
    }

    static double unsafeFloatToDouble(Evaluator eval, Object fusionFloat)
        throws FusionException
    {
        return ((BaseFloat) fusionFloat).primitiveDoubleValue();
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
        throws FusionException, ArgumentException
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
        throws FusionException, ArgumentException
    {
        Object arg = args[argNum];
        if (arg instanceof LongInt)
        {
            return ((LongInt) arg).myContent;
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
        throws FusionException, ArgumentException
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
        throws FusionException, ArgumentException
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
        throws FusionException, ArgumentException
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
        throws FusionException, ArgumentException
    {
        Object arg = args[argNum];
        if (arg instanceof BaseDecimal)
        {
            return ((BaseDecimal) arg).toBigDecimal();
        }

        throw who.argFailure(expectation, argNum, args);
    }

    static BigDecimal checkNullableDecimalArg(Evaluator eval,
                                              Procedure who,
                                              int       argNum,
                                              Object... args)
        throws FusionException, ArgumentException
    {
        String expectation = "nullable decimal";
        BigDecimal result = checkDecimalArg(eval, who, expectation, argNum, args);
        return result;
    }


    /**
     * @return not null
     */
    static BigDecimal checkRequiredDecimalArg(Evaluator eval,
                                              Procedure who,
                                              int       argNum,
                                              Object... args)
        throws FusionException, ArgumentException
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
    static BaseFloat checkFloatArg(Evaluator eval,
                                   Procedure who,
                                   String    expectation,
                                   int       argNum,
                                   Object... args)
        throws FusionException, ArgumentException
    {
        Object arg = args[argNum];
        if (arg instanceof BaseFloat)
        {
            return (BaseFloat) arg;
        }

        throw who.argFailure(expectation, argNum, args);
    }


    static Double checkNullableFloatArg(Evaluator eval,
                                        Procedure who,
                                        int       argNum,
                                        Object... args)
        throws FusionException, ArgumentException
    {
        String expectation = "nullable float";
        BaseFloat f = checkFloatArg(eval, who, expectation, argNum, args);
        return f.objectDoubleValue();
    }


    /**
     * @return not null
     */
    static double checkRequiredFloatArg(Evaluator eval,
                                        Procedure who,
                                        int       argNum,
                                        Object... args)
        throws FusionException, ArgumentException
    {
        String expectation = "non-null float";

        BaseFloat f = checkFloatArg(eval, who, expectation, argNum, args);
        if (f.isAnyNull())
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return f.primitiveDoubleValue();
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


    static final class SumProc
        extends Procedure
    {
        SumProc()
        {
            //    "                                                                               |
            super("Returns the sum of the `num`bers, which must be int or decimal.  With no\n" +
                  "arguments, returns integer 0.",
                  "num", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            int arity = args.length;

            if (arity == 0) return ZERO_INT;

            Object arg0 = args[0];
            if (arg0 instanceof BaseInt || arg0 instanceof BaseDecimal)
            {
                if (arity == 1) return arg0;

                BaseNumber accum = (BaseNumber) arg0;

                for (int i = 1; i < arity; i++)
                {
                    Object arg = args[i];
                    if (arg instanceof BaseInt || arg instanceof BaseDecimal)
                    {
                        BaseNumber num = (BaseNumber) arg;
                        if (! num.isAnyNull())
                        {
                            accum = accum.add(num);
                            continue;
                        }
                    }

                    throw argFailure("non-null int or decimal", i, args);
                }

                return accum;
            }

            throw argFailure("non-null int or decimal", 0, args);
        }
    }


    static final class ProductProc
        extends Procedure
    {
        ProductProc()
        {
            //    "                                                                               |
            super("Returns the product of the `num`bers, which must be int or decimal.  With no\n" +
                  "arguments, returns integer 1.",
                  "num", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            int arity = args.length;

            if (arity == 0) return ONE_INT;

            Object arg0 = args[0];
            if (arg0 instanceof BaseInt || arg0 instanceof BaseDecimal)
            {
                if (arity == 1) return arg0;

                BaseNumber accum = (BaseNumber) arg0;

                for (int i = 1; i < arity; i++)
                {
                    Object arg = args[i];
                    if (arg instanceof BaseInt || arg instanceof BaseDecimal)
                    {
                        BaseNumber num = (BaseNumber) arg;
                        if (! num.isAnyNull())
                        {
                            accum = accum.multiply(num);
                            continue;
                        }
                    }

                    throw argFailure("non-null int or decimal", i, args);
                }

                return accum;
            }

            throw argFailure("non-null int or decimal", 0, args);
        }
    }


    static final class DifferenceProc
        extends Procedure
    {
        DifferenceProc()
        {
            //    "                                                                               |
            super("With two or more int or decimal `num`bers, returns their difference,\n" +
                  "associating to the left.  With one int or decimal argument, returns its\n" +
                  "negation.",
                  "num", DOTDOTDOTPLUS);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityAtLeast(1, args);

            Object arg0 = args[0];
            if (arg0 instanceof BaseInt || arg0 instanceof BaseDecimal)
            {
                BaseNumber accum = (BaseNumber) arg0;

                int arity = args.length;
                if (arity == 1)
                {
                    return ZERO_INT.subtract(accum);
                }
                else
                {
                    for (int i = 1; i < arity; i++)
                    {
                        Object arg = args[i];
                        if (arg instanceof BaseInt || arg instanceof BaseDecimal)
                        {
                            BaseNumber num = (BaseNumber) arg;
                            if (! num.isAnyNull())
                            {
                                accum = accum.subtract(num);
                                continue;
                            }
                        }

                        throw argFailure("non-null int or decimal", i, args);
                    }

                    return accum;
                }
            }

            throw argFailure("non-null int or decimal", 0, args);
        }
    }


    static class DivideProc
        extends Procedure
    {
        DivideProc()
        {
            //    "                                                                               |
            super("Returns a decimal whose numeric value is `(dividend / divisor)`.  Both\n" +
                  "arguments must be decimals.  An exception is thrown if the result cannot be\n" +
                  "represented exactly.",
                  "dividend", "divisor");
        }

        /**
         * Helper to allow subclass to tweak the divison context.
         */
        BigDecimal divide(Evaluator eval, Object[] args,
                          BigDecimal dividend, BigDecimal divisor)
            throws FusionException
        {
            try
            {
                return dividend.divide(divisor);
            }
            catch (ArithmeticException e)
            {
                String message =
                    "Result of division isn't exact.\n" +
                    "Arguments were:\n  " + safeWriteToString(eval, args[0]) +
                    "\n  " + safeWriteToString(eval, args[1]);
                throw contractFailure(message, e);
            }
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            BigDecimal dividend = checkRequiredDecimalArg(eval, this, 0, args);
            BigDecimal divisor  = checkRequiredDecimalArg(eval, this, 1, args);

            BigDecimal result = divide(eval, args, dividend, divisor);
            return makeDecimal(eval, result);
        }
    }


    /** EXPERIMENTAL **/
    static final class DecimalDivideProc
        extends DivideProc
    {
        @Override
        BigDecimal divide(Evaluator eval, Object[] args,
                          BigDecimal dividend, BigDecimal divisor)
            throws FusionException
        {
            return dividend.divide(divisor, MathContext.DECIMAL128);
        }
    }


    static final class CeilingProc
        extends Procedure1
    {
        CeilingProc()
        {
            //    "                                                                               |
            super("Returns the smallest int greater than or equal to `number` (that is, truncate\n" +
                  "toward positive infinity). The input must be a non-null int or decimal, and\n" +
                  "the result is an int.",
                  "number");
        }

        @Override
        Object doApply(Evaluator eval, Object arg0)
            throws FusionException
        {
            if (isAnyNull(eval, arg0).isFalse())
            {
                if (isDecimal(eval, arg0))
                {
                    BigDecimal d = unsafeNumberToBigDecimal(eval, arg0);
                    BigInteger i = d.setScale(0, CEILING).toBigInteger();
                    return makeInt(eval, i);
                }

                if (isInt(eval, arg0))
                {
                    return arg0;
                }
            }

            throw new ArgumentException(this, "non-null int or decimal", 0, arg0);
        }
    }


    static final class FloorProc
        extends Procedure1
    {
        FloorProc()
        {
            //    "                                                                               |
            super("Returns the largest int less than or equal to `number` (that is, truncate\n" +
                  "toward negative infinity). The input must be a non-null int or decimal, and the\n" +
                  "result is an int.",
                  "number");
        }

        @Override
        Object doApply(Evaluator eval, Object arg0)
            throws FusionException
        {
            if (isAnyNull(eval, arg0).isFalse())
            {
                if (isDecimal(eval, arg0))
                {
                    BigDecimal d = unsafeNumberToBigDecimal(eval, arg0);
                    BigInteger i = d.setScale(0, FLOOR).toBigInteger();
                    return makeInt(eval, i);
                }

                if (isInt(eval, arg0))
                {
                    return arg0;
                }
            }

            throw new ArgumentException(this, "non-null int or decimal", 0, arg0);
        }
    }
}
