// Copyright (c) 2018-2021 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.util.hamt;

import static com.amazon.fusion.util.hamt.FunctionalHashTrie.empty;
import static com.amazon.fusion.util.hamt.FunctionalHashTrie.fromArrays;
import static com.amazon.fusion.util.hamt.FunctionalHashTrie.fromEntries;
import static com.amazon.fusion.util.hamt.FunctionalHashTrie.fromMap;
import static com.amazon.fusion.util.hamt.FunctionalHashTrie.fromSelectedKeys;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isIn;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import com.amazon.fusion.util.hamt.FunctionalHashTrie.Changes;
import java.util.AbstractMap.SimpleEntry;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class FunctionalHashTrieTest
{
    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private HashMap<Object, Object>            baselineMap;
    private FunctionalHashTrie<Object, Object> fht;

    public void setup(int size)
    {
        baselineMap = new HashMap<>();
        for (int i = 0; i < size; i++)
        {
            baselineMap.put(new Object(), new Object());
        }
        fht = fromMap(baselineMap);
    }

    private void doRemoval()
    {
        Object[] keys = baselineMap.keySet().toArray();
        Set<Object> keysToRemove = new LinkedHashSet<>(keys.length / 2);
        for (int i = 0; i < keys.length / 2; i++)
        {
            keysToRemove.add(keys[i]);
        }

        baselineMap.keySet().removeAll(keysToRemove);

        for (Object key : keysToRemove)
        {
            FunctionalHashTrie<Object, Object> newHash = fht.withoutKey(key);
            assertNotEquals(fht, newHash);
            fht = newHash;
        }
    }

    private void doAddition()
    {
        int toAdd = baselineMap.size();
        for (int i = 0; i < toAdd; i++)
        {
            Object key   = new Object();
            Object value = new Object();

            baselineMap.put(key, value);
            FunctionalHashTrie<Object, Object> newHash = fht.with1(key, value);
            assertNotEquals(fht, newHash);
            fht = newHash;
        }
    }

    private void checkSizing()
    {
        assertEquals(baselineMap.size(), fht.size());
    }

    private void checkKeys()
    {
        Set<Object> keySet = fht.keySet();
        Set<Object> baselineSet = baselineMap.keySet();
        assertEquals(baselineSet, keySet);
    }

    private void compareEntries()
    {
        Map<Object, Object> remainder = (Map<Object, Object>) baselineMap.clone();

        for (Map.Entry<Object, Object> entry : fht)
        {
            assertEquals(entry.getValue(), remainder.remove(entry.getKey()));
        }

        assertEquals("elements remaining", 0, remainder.size());
    }

    private void compareWithBaseline()
    {
        checkSizing();
        checkKeys();
        compareEntries();
    }

    private void performTests()
    {
        compareWithBaseline();
        doRemoval();
        compareWithBaseline();
        doAddition();
        compareWithBaseline();
    }

    @Test
    public void checkEmpty()
    {
        setup(0);
        assertTrue(fht.isEmpty());
        assertSame(empty(), fht);
        performTests();

        FunctionalHashTrie without = fht.withoutKey("anything");
        assertSame(without, fht);
        assertSame(empty(), fht);
    }

    @Test
    public void checkSingle()
    {
        setup(1);
        performTests();
    }

    @Test
    public void checkSmall()
    {
        setup(10);
        performTests();
    }

    @Test
    public void checkVeryLarge()
    {
        setup(10000);
        performTests();
    }

    @Test
    public void checkVeryVeryLarge()
    {
        setup(1000000);
        performTests();
    }


    @Test
    public void checkNullKeys()
    {
        setup(1);

        final String failureMessage = "Should have raised a NullPointerException";
        try
        {
            fht.with1(null, "foo");
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.with1("foo", null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.with1(null, null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.withoutKey(null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.get(null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }

        try
        {
            fht.containsKey(null);
            fail(failureMessage);
        }
        catch (NullPointerException e)
        {
            // expected
        }
    }


    //=========================================================================
    // Expectations and fixture setup

    private void checkChanges(int changeCount, int delta, Changes changes)
    {
        assertEquals("keys changed", changeCount, changes.changeCount());
        assertEquals("key delta",    delta,       changes.keyCountDelta());
    }


    static void expectEmpty(MultiHashTrie t)
    {
        assertThat(t.isEmpty(), is(true));
        assertTrue(t.isEmpty());
        assertEquals(0, t.keyCount());
        assertThat(t.keyCount(), is(0));
        assertSame(empty(), t);
        assertThat(t, sameInstance((MultiHashTrie) empty()));
    }


    static void expectMapping(MultiHashTrie<Object, Object> hash,
                              Object key,
                              Object... expectedValues)
    {
        assertThat(hash.containsKey(key), is(true));
        assertThat(hash.get(key), isIn(expectedValues));
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


    FunctionalHashTrie simpleSubject()
    {
        return hash1(1, 1);
    }


    //=========================================================================
    // Creation

    @Test
    public void compareMutableCreateVsSequential()
    {
        setup(10000);

        FunctionalHashTrie seq = empty();
        for (Map.Entry e : fht)
        {
            seq = seq.with1(e.getKey(), e.getValue());
        }

        assertEquals(fht, seq);
        assertEquals(fht.size(), seq.size());
        for (Map.Entry e : fht)
        {
            Object key = e.getKey();
            assertEquals(fht.get(key), seq.get(key));
        }
    }


    // fromMap()

    @Test
    public void fromMapGivenEmptyMapReturnsEmptySingleton()
    {
        FunctionalHashTrie h = fromMap(Collections.emptyMap());
        expectEmpty(h);
    }

    @Test
    public void fromMapGivenValidInputReturnsHash()
    {
        Map m = new HashMap();
        m.put(1, 1);
        m.put(2, 2);
        m.put(3, 3);

        FunctionalHashTrie h = fromMap(m);
        assertThat(h, is(hash1(1, 1, 2, 2, 3, 3)));
    }


    // fromEntries()

    @Test
    public void fromEntriesGivenEmptyIteratorReturnsEmptySingleton()
    {
        FunctionalHashTrie h = fromEntries(Collections.<Map.Entry<Object,Object>>emptyIterator());
        expectEmpty(h);
    }

    @Test
    public void fromEntriesGivenValidInputReturnsHash()
    {
        Map m = new HashMap();
        m.put(1, 1);
        m.put(2, 2);
        m.put(3, 3);

        FunctionalHashTrie h = fromEntries(m.entrySet().iterator());
        assertThat(h, is(hash1(1, 1, 2, 2, 3, 3)));
    }


    // fromArrays()

    @Test
    public void fromArraysGivenEmptyArraysReturnsEmptySingleton()
    {
        Changes changes = new Changes();

        FunctionalHashTrie t = fromArrays(new Object[0], new Object[0], changes);

        assertSame(empty(), t);
        checkChanges(0, 0, changes);
    }

    @Test
    public void fromArraysRequiresEqualLengthArrays()
    {
        thrown.expect(IllegalArgumentException.class);
        fromArrays(new Object[] { 1 }, new Object[] { 1, 2 });
    }

    @Test
    public void fromArraysZips()
    {
        FunctionalHashTrie t = fromArrays(new Object[] { 1, 2 },
                                          new Object[] { 3, 4 });
        assertThat(t, is(hash1(1, 3, 2, 4)));

        t = fromArrays(new Object[] { 5, 6, 5 },
                       new Object[] { 8, 9, 0 });
        assertThat(t, is(hash1(5, 0, 6, 9)));
    }

    @Test
    public void fromArraysReusesChanges()
    {
        Changes changes = new Changes();

        FunctionalHashTrie t = fromArrays(new Object[] { 1, 2 },
                                          new Object[] { 3, 4 },
                                          changes);

        checkChanges(2, 2, changes);
        assertEquals(3, t.get(1));
        assertEquals(4, t.get(2));

        t = fromArrays(new Object[] { 5, 6, 5 },
                       new Object[] { 8, 9, 0 },
                       changes);

        checkChanges(5, 4, changes);
        assertEquals(0, t.get(5));
        assertEquals(9, t.get(6));
    }


    // fromSelectedKeys()

    @Test
    public void fromSelectedKeysReturnsSubset()
    {
        FunctionalHashTrie origin = hash1(1, 1, 2, 2, 3, 3);

        FunctionalHashTrie t = fromSelectedKeys(origin, new Object[]{ 1, 3, 5 });

        assertThat(t, is(hash1(1, 1, 3, 3)));
    }

    @Test
    public void fromSelectedKeysReturnsEmptySingleton()
    {
        FunctionalHashTrie origin = hash1(1, 1, 2, 2);

        FunctionalHashTrie t = fromSelectedKeys(origin, new Object[]{});
        expectEmpty(t);

        t = fromSelectedKeys(origin, new Object[]{ 3, 4 });
        expectEmpty(t);
    }


    //=========================================================================
    // Modification

    // with1()

    @Test
    public void with1ReplacesExistingValues()
    {
        expectMapping(simpleSubject().with1(1, 3), 1, 3);
    }

    @Test
    public void with1GivenSameValueReturnsSelf()
    {
        FunctionalHashTrie s = simpleSubject();
        assertThat(s.with1(1, s.get(1)), sameInstance(s));
    }

    @Test
    public void testWithReusingChanges()
    {
        FunctionalHashTrie trie = empty();

        Changes changes = new Changes();
        trie = trie.with(1, 1, changes);
        checkChanges(1, 1, changes);
        assertEquals(1, trie.size());

        trie = trie.with(2, 2, changes);
        checkChanges(2, 2, changes);
        assertEquals(2, trie.size());

        trie = trie.with(2, 3, changes);
        checkChanges(3, 2, changes);
        assertEquals(2, trie.size());
    }


    // withoutKey()

    @Test
    public void withoutKeyRemovesSingleValue()
    {
        FunctionalHashTrie subject = hash1(1, 1, 2, 2, 3, 3);

        FunctionalHashTrie r = subject.withoutKey(3);
        assertThat(r, is(hash1(1, 1, 2, 2)));
        assertThat(r, not(subject));
    }


    // withoutKeys()

    @Test
    public void withoutKeysGivenKeysReturnsSubset()
    {
        FunctionalHashTrie origin = hash1(1, 1, 2, 2, 3, 3);
        FunctionalHashTrie t = origin.withoutKeys(1, 3, 5);

        assertThat(t, is(hash1(2, 2)));
        assertThat(t, not(origin));
    }

    @Test
    public void withoutKeysGivenAllKeysReturnsSingleton()
    {
        FunctionalHashTrie origin = empty().with1(1, 1).with1(2, 2);
        FunctionalHashTrie t = origin.withoutKeys(new Object[]{1, 2});
        assertSame(empty(), t);
    }


    // merge1()

    @Test
    public void merge1GivenEmptyReturnsSelf()
    {
        FunctionalHashTrie s = simpleSubject();
        FunctionalHashTrie r = s.merge1(empty());
        assertThat(r, sameInstance(s));
    }

    @Test
    public void merge1GivenValidInputReturnsSingleHash()
    {
        FunctionalHashTrie h1 = hash1(10, 1, 11, 1, 12, 1, 20, 2, 21, 2, 22, 2);
        FunctionalHashTrie h2 = hash1(1, 5, 2, 6, 11, 5, 12, 6, 21, 5, 22, 6);

        FunctionalHashTrie r = h1.merge1(h2);
        assertThat(r, is(hash1(1, 5, 2, 6, 10, 1, 11, 5, 12, 6, 20, 2, 21, 5, 22, 6)));
    }

    @Test
    public void testMergeInvokesChanges()
    {
        Changes changes = new Changes() {
            @Override
            public Object inserting(Object givenValue)
            {
                return givenValue;
            }

            @Override
            public Object replacing(Object oldValue, Object givenValue)
            {
                assert givenValue.equals("new");
                return oldValue;
            }
        };

        Map.Entry[] entries = { new SimpleEntry<>("f", "old"),
            new SimpleEntry<>("f", "new") };
        FunctionalHashTrie trie = fromEntries(entries, changes);
        assertEquals(1, trie.size());
        assertEquals("old", trie.get("f"));
    }

    @Test
    public void testMergeReusingChanges()
    {
        FunctionalHashTrie empty = empty();

        Changes changes = new Changes();
        FunctionalHashTrie trie1 = empty.with(1, 1, changes);

        // Replace the existing key, so the delta stays the same.
        FunctionalHashTrie trie2 = trie1.merge(empty.with1(1, 2), changes);
        checkChanges(2, 1, changes);
        assertEquals(1, trie2.size());
    }


    // transform()

    @Test
    public void transformReturnsEmpty()
    {
        FunctionalHashTrie h1 = hash1(1, 3, 3, 3, 4, 6, 5, 15);
        expectEmpty(h1.transform(new TransformTestCase.Remove3sIncrement2sXform()));
    }

    @Test
    public void transformGivenNoopReturnsSame()
    {
        FunctionalHashTrie h = hash1(1, 1, 2, 1, 4, 5);
        assertThat(h.transform(new TransformTestCase.Remove3sIncrement2sXform()),
                   sameInstance(h));
    }

    @Test
    public void transformMixedChanges()
    {
        FunctionalHashTrie h = hash1(1, 1, 2, 2, 3, 3, 4, 1, 5, 1, 6, 2, 7, 3, 8, 3);
        assertThat(h.transform(new TransformTestCase.Remove3sIncrement2sXform()),
                   is(hash1(1, 1, 2, 3, 4, 1, 5, 1, 6, 3)));
    }
}
