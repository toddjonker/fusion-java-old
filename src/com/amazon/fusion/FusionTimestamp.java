// Copyright (c) 2012-2015 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.falseBool;
import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionBool.trueBool;
import static com.amazon.fusion.FusionNumber.checkIntArgToJavaInt;
import static com.amazon.fusion.FusionNumber.isIntOrDecimal;
import static com.amazon.fusion.FusionNumber.makeDecimal;
import static com.amazon.fusion.FusionNumber.unsafeNumberToBigDecimal;
import static com.amazon.fusion.FusionString.checkNullableStringArg;
import static com.amazon.fusion.FusionString.makeString;
import static com.amazon.fusion.FusionSymbol.BaseSymbol.internSymbols;
import static com.amazon.fusion.SimpleSyntaxValue.makeSyntax;
import static com.amazon.ion.Timestamp.UTC_OFFSET;
import static com.amazon.ion.util.IonTextUtils.isDigit;
import com.amazon.fusion.FusionBool.BaseBool;
import com.amazon.fusion.FusionSymbol.BaseSymbol;
import com.amazon.ion.IonException;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.Timestamp;
import com.amazon.ion.ValueFactory;
import java.io.IOException;
import java.math.BigDecimal;


final class FusionTimestamp
{
    private FusionTimestamp() {}



    //========================================================================
    // Representation


    abstract static class BaseTimestamp
        extends BaseValue
    {
        private BaseTimestamp() {}

        abstract Timestamp timestampValue();

        @Override
        final boolean isAnnotatable()
        {
            return true;
        }

        @Override
        BaseTimestamp annotate(Evaluator eval, BaseSymbol[] annotations)
        {
            if (annotations.length == 0) return this;
            return new AnnotatedTimestamp(annotations, this);
        }

        @Override
        SyntaxValue datumToSyntaxMaybe(Evaluator eval, SourceLocation loc)
        {
            return makeSyntax(eval, loc, this);
        }
    }


    private static class NullTimestamp
        extends BaseTimestamp
    {
        private NullTimestamp() {}

        @Override
        Timestamp timestampValue()
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
            boolean b = (right instanceof BaseTimestamp
                         && ((BaseTimestamp) right).isAnyNull());
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
            return factory.newNullTimestamp();
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeNull(IonType.TIMESTAMP);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append("null.timestamp");
        }
    }


    private static class ActualTimestamp
        extends BaseTimestamp
    {
        private final Timestamp myContent;

        private ActualTimestamp(Timestamp content)
        {
            assert content != null;
            myContent = content;
        }

        @Override
        Timestamp timestampValue()
        {
            return myContent;
        }

        @Override
        BaseBool strictEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseTimestamp)
            {
                Timestamp rt = ((BaseTimestamp) right).timestampValue();
                if (myContent.equals(rt))
                {
                    return trueBool(eval);
                }
            }

            return falseBool(eval);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseTimestamp)
            {
                Timestamp rt = ((BaseTimestamp) right).timestampValue();
                if (rt != null && myContent.compareTo(rt) == 0)
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
            return factory.newTimestamp(myContent);
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.writeTimestamp(myContent);
        }

        @Override
        void write(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            myContent.print(out);
        }

        @Override
        void display(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            myContent.print(out);
        }
    }


    private static class AnnotatedTimestamp
        extends BaseTimestamp
    {
        /** Not null or empty */
        final BaseSymbol[] myAnnotations;

        /** Not null, and not AnnotatedTimestamp */
        final BaseTimestamp  myValue;

        private AnnotatedTimestamp(BaseSymbol[] annotations,
                                   BaseTimestamp value)
        {
            assert annotations.length != 0;
            myAnnotations = annotations;
            myValue = value;
        }

        @Override
        final boolean isAnnotated()
        {
            return true;
        }

        @Override
        BaseSymbol[] getAnnotations()
        {
            return myAnnotations;
        }

        @Override
        BaseTimestamp annotate(Evaluator eval, BaseSymbol[] annotations)
        {
            return myValue.annotate(eval, annotations);
        }

        @Override
        boolean isAnyNull() { return myValue.isAnyNull(); }

        @Override
        Timestamp timestampValue()
        {
            return myValue.timestampValue();
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
        IonValue copyToIonValue(ValueFactory factory,
                                boolean throwOnConversionFailure)
            throws FusionException, IonizeFailure
        {
            IonValue iv = myValue.copyToIonValue(factory,
                                                 throwOnConversionFailure);
            iv.setTypeAnnotations(getAnnotationsAsJavaStrings());
            return iv;
        }

        @Override
        void ionize(Evaluator eval, IonWriter out)
            throws IOException, IonException, FusionException, IonizeFailure
        {
            out.setTypeAnnotations(getAnnotationsAsJavaStrings());
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


    private static final BaseTimestamp NULL_TIMESTAMP = new NullTimestamp();


    /**
     * @param value may be null to make {@code null.timestamp}.
     *
     * @return not null.
     */
    static BaseTimestamp makeTimestamp(Evaluator eval, Timestamp value)
    {
        return (value == null ? NULL_TIMESTAMP : new ActualTimestamp(value));
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null to make {@code null.timestamp}.
     *
     * @return not null.
     */
    static BaseTimestamp makeTimestamp(Evaluator eval,
                                       String[]  annotations,
                                       Timestamp value)
    {
        BaseTimestamp base = makeTimestamp(eval, value);
        return base.annotate(eval, internSymbols(annotations));
    }


    /**
     * @param fusionTimestamp must be a Fusion timestamp.
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     *
     * @return not null.
     */
    static BaseTimestamp unsafeTimestampAnnotate(Evaluator eval,
                                                 Object    fusionTimestamp,
                                                 String[]  annotations)
    {
        BaseTimestamp base = (BaseTimestamp) fusionTimestamp;
        return base.annotate(eval, internSymbols(annotations));
    }


    //========================================================================
    // Predicates


    public static boolean isTimestamp(TopLevel top, Object value)
        throws FusionException
    {
        return (value instanceof BaseTimestamp);
    }

    static boolean isTimestamp(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof BaseTimestamp);
    }


    //========================================================================
    // Conversions


    /**
     * @param fusionTimestamp must be a Fusion timestamp.
     *
     * @return null if given {@code null.timestamp}.
     */
    static Timestamp unsafeTimestampToJavaTimestamp(Evaluator eval,
                                                    Object fusionTimestamp)
        throws FusionException
    {
        return ((BaseTimestamp) fusionTimestamp).timestampValue();
    }


    /**
     * Converts a Fusion timestamp to a {@link Timestamp}.
     *
     * @return null if the value isn't a Fusion timestamp.
     */
    static Timestamp timestampToJavaTimestamp(Evaluator eval, Object value)
        throws FusionException
    {
        if (isTimestamp(eval, value))
        {
            return unsafeTimestampToJavaTimestamp(eval, value);
        }
        return null;
    }


    //========================================================================
    // Procedure Helpers


    /**
     * @param expectation must not be null.
     * @return may be null
     */
    static Timestamp checkTimestampArg(Evaluator eval,
                                       Procedure who,
                                       String    expectation,
                                       int       argNum,
                                       Object... args)
        throws FusionException, ArgumentException
    {
        Object arg = args[argNum];
        if (arg instanceof BaseTimestamp)
        {
            return ((BaseTimestamp) arg).timestampValue();
        }

        throw who.argFailure(expectation, argNum, args);
    }


    static Timestamp checkNullableTimestampArg(Evaluator eval,
                                               Procedure who,
                                               int argNum,
                                               Object... args)
        throws FusionException, ArgumentException
    {
        String expectation = "nullable timestamp";
        return checkTimestampArg(eval, who, expectation, argNum, args);
    }


    @Deprecated
    static Timestamp checkNullableArg(Procedure who, int argNum,
                                      Object... args)
        throws FusionException, ArgumentException
    {
        return checkNullableTimestampArg(null, who, argNum, args);
    }


    /**
     * @return not null
     */
    static Timestamp checkRequiredTimestampArg(Evaluator eval,
                                               Procedure who,
                                               int       argNum,
                                               Object... args)
        throws FusionException, ArgumentException
    {
        String expectation = "non-null timestamp";
        Timestamp result =
            checkTimestampArg(eval, who, expectation, argNum, args);
        if (result == null)
        {
            throw who.argFailure(expectation, argNum, args);
        }
        return result;
    }



    //========================================================================
    // Procedures


    static final class IsTimestampProc
        extends Procedure1
    {
        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            boolean r = isTimestamp(eval, arg);
            return makeBool(eval, r);
        }
    }


    static final class StringToTimestampProc
        extends Procedure1
    {
        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            String input = checkNullableStringArg(eval, this, 0, arg);

            Timestamp result;
            if (input == null)
            {
                result = null;
            }
            else
            {
                try
                {
                    result = Timestamp.valueOf(input);

                    // Don't accept input "null.timestamp". That's shady.
                    if (result == null)
                    {
                        throw argFailure("timestamp-formatted string", 0, arg);
                    }

                    // Hack around Timestamp.valueOf() accepting stoppers.
                    char last = input.charAt(input.length() - 1);
                    if (! (last == 'Z' || last == 'T' || isDigit(last, 10)))
                    {
                        throw argFailure("timestamp-formatted string", 0, arg);
                    }
                }
                catch (IllegalArgumentException e)
                {
                    throw argFailure("timestamp-formatted string", 0, arg);
                }
            }

            return makeTimestamp(eval, result);
        }
    }


    static final class TimestampToStringProc
        extends Procedure1
    {
        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            Timestamp input = checkNullableTimestampArg(eval, this, 0, arg);

            String result = (input == null ? null : input.toString());
            return makeString(eval, result);
        }
    }


    static final class TimestampNowProc
        extends Procedure0
    {
        @Override
        Object doApply(Evaluator eval)
            throws FusionException
        {
            Timestamp now = Timestamp.now();
            return makeTimestamp(eval, now);
        }
    }


    static final class TimestampToEpochMillisProc
        extends Procedure1
    {
        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            Timestamp ts = checkRequiredTimestampArg(eval, this, 0, arg);
            BigDecimal millis = ts.getDecimalMillis();
            return makeDecimal(eval, millis);
        }
    }


    static final class EpochMillisToTimestampProc
        extends Procedure1
    {
        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            if (! isIntOrDecimal(eval, arg) || isAnyNull(eval, arg).isTrue())
            {
                throw argFailure("non-null int or decimal", 0, arg);
            }

            BigDecimal epochMillis = unsafeNumberToBigDecimal(eval, arg);
            Timestamp ts = Timestamp.forMillis(epochMillis, UTC_OFFSET);
            return makeTimestamp(eval, ts);
        }
     }


    private abstract static class UnsafeTimestampAddProc
        extends Procedure2
    {
        abstract Timestamp add(Timestamp timestamp, int period);

        @Override
        final Object doApply(Evaluator eval, Object timestamp, Object period)
            throws FusionException
        {
            Timestamp ts = unsafeTimestampToJavaTimestamp(eval, timestamp);
            int p = checkIntArgToJavaInt(eval, this, 1, timestamp, period);
            ts = add(ts, p);
            return makeTimestamp(eval, ts);
        }
    }

    static final class UnsafeTimestampAddYearProc
        extends UnsafeTimestampAddProc
    {
        @Override
        Timestamp add(Timestamp timestamp, int period)
        {
            return timestamp.addYear(period);
        }
    }

    static final class UnsafeTimestampAddMonthProc
        extends UnsafeTimestampAddProc
    {
        @Override
        Timestamp add(Timestamp timestamp, int period)
        {
            return timestamp.addMonth(period);
        }
    }

    static final class UnsafeTimestampAddDayProc
        extends UnsafeTimestampAddProc
    {
        @Override
        Timestamp add(Timestamp timestamp, int period)
        {
            return timestamp.addDay(period);
        }
    }

    static final class UnsafeTimestampAddHourProc
        extends UnsafeTimestampAddProc
    {
        @Override
        Timestamp add(Timestamp timestamp, int period)
        {
            return timestamp.addHour(period);
        }
    }

    static final class UnsafeTimestampAddMinuteProc
        extends UnsafeTimestampAddProc
    {
        @Override
        Timestamp add(Timestamp timestamp, int period)
        {
            return timestamp.addMinute(period);
        }
    }

    static final class UnsafeTimestampAddSecondProc
        extends UnsafeTimestampAddProc
    {
        @Override
        Timestamp add(Timestamp timestamp, int period)
        {
            return timestamp.addSecond(period);
        }
    }
}
