// Copyright (c) 2021 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.util.hamt;

import static com.amazon.fusion.util.hamt.FunctionalHashTrie.fromEntries;
import static com.amazon.fusion.util.hamt.MultiHashTrie.empty;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isIn;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.util.AbstractMap;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map.Entry;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

@SuppressWarnings({"rawtypes", "unchecked"})
public abstract class MultiHashTrieTestCase
{
    @Rule
    public ExpectedException thrown = ExpectedException.none();


    static void expectEmpty(MultiHashTrie t)
    {
        assertTrue(t.isEmpty());
        assertThat(t.keyCount(), is(0));
        assertThat(t, sameInstance((MultiHashTrie) empty()));
        assertTrue(t.equals(empty()));
        assertTrue(empty().equals(t));
    }


    static void expectMapping(MultiHashTrie<Object, Object> hash,
                              Object key,
                              Object... expectedValues)
    {
        assertThat(hash.containsKey(key), is(true));
        assertThat(hash.get(key), isIn(expectedValues));
        assertThat(hash.getMulti(key), containsInAnyOrder(expectedValues));
    }


    /**
     * Wraps {@link FunctionalHashTrie#with1(Object, Object)} with validation.
     */
    static FunctionalHashTrie with1(FunctionalHashTrie h, Object k, Object v)
    {
        Object priorValue = h.get(k);

        FunctionalHashTrie r = h.with1(k, v);
        expectMapping(r, k, v);

        if (priorValue == v)
        {
            assertSame(h, r);
        }

        return r;
    }


    static FunctionalHashTrie hash1(Object... kvPairs)
    {
        assertEquals(0, kvPairs.length % 2);

        FunctionalHashTrie hash = empty();
        for (int i = 0; i < kvPairs.length; i += 2)
        {
            hash = with1(hash, kvPairs[i], kvPairs[i+1]);
        }

        // Test iteration and construction through another route.
        assertEquals(hash, fromEntries(hash.iterator()));

        return hash;
    }


    static Entry entry(Object k, Object v)
    {
        return new AbstractMap.SimpleEntry(k, v);
    }

    static Iterator iterate(Object... values)
    {
        return Arrays.asList(values).iterator();
    }


    /**
     * Return a simple hash of the relevant subtype (multi/single/empty).
     */
    abstract MultiHashTrie simpleSubject();


    //=========================================================================
    // Inspection
    //   These are exercised extensively via hash() and other fixture methods.

    // containsKey()

    @Test
    public void containsKeyRejectsNullKey()
    {
        thrown.expect(NullPointerException.class);
        simpleSubject().containsKey(null);
    }


    // get() -- Exercised via hash() and other fixture methods.

    @Test
    public void getRejectsNullKey()
    {
        thrown.expect(NullPointerException.class);
        simpleSubject().get(null);
    }


    //=========================================================================
    // Modification

    // with1()

    @Test
    public void with1RejectsNullKey()
    {
        thrown.expect(NullPointerException.class);
        simpleSubject().with1(null, 1);
    }

    @Test
    public void with1RejectsNullValue()
    {
        thrown.expect(NullPointerException.class);
        simpleSubject().with1(2, null);
    }


    // withoutKey()

    @Test
    public void withoutKeyRejectsNullKey()
    {
        thrown.expect(NullPointerException.class);
        simpleSubject().withoutKey(null);
    }

    @Test
    public void withoutKeyGivenAbsentKeyReturnsSelf()
    {
        MultiHashTrie subject = simpleSubject();
        assertFalse(subject.containsKey(-1));
        assertThat(subject.withoutKey(-1), sameInstance(subject));
    }


    // withoutKeys()

    @Test
    public void withoutKeysRejectsNullArray()
    {
        thrown.expect(NullPointerException.class);
        simpleSubject().withoutKeys((Object[]) null);
    }

    @Test
    public void withoutKeysRejectsNullKey()
    {
        thrown.expect(NullPointerException.class);
        simpleSubject().withoutKeys(1, null, 3);
    }

    @Test
    public void withoutKeysGivenAbsentKeysReturnsSelf()
    {
        MultiHashTrie s = simpleSubject();
        assertFalse(s.containsKey(-1));
        assertFalse(s.containsKey(-2));
        assertThat(s.withoutKeys(-1, -2), sameInstance(s));
    }

    @Test
    public void withoutKeysGivenAllKeysReturnsEmpty()
    {
        MultiHashTrie s = simpleSubject();
        Object[]      keys = s.keySet().toArray();
        expectEmpty(s.withoutKeys(keys));
    }


    // oneify()

    static void checkOneify(MultiHashTrie h)
    {
        FunctionalHashTrie<Object, Object> r = h.oneify();
        assertThat(r.keyCount(), is(h.keyCount()));

        for (Entry<Object, Object> entry : r)
        {
            Object key = entry.getKey();
            assertThat(entry.getValue(), isIn(h.getMulti(key)));
            h = h.withoutKey(key); // So we don't match the key again.
        }

        assertThat(h.size(), is(0));
    }
}
