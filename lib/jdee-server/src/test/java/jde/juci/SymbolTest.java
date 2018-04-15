/*
 * Copyright (C) 2002 by Nick Sieger
 *
 * $Revision: 1.1 $
 * $Date: 2003/02/15 20:58:26 $
 *
 * Author: Nick Sieger <nsieger@bitstream.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package jde.juci;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test name conversion algorithms in the {@link Symbol} class.
 *
 * @author <a href="mailto:nsieger@bitstream.net">Nick Sieger</a>
 * @version 1.0
 */
public class SymbolTest {

    @Test
    public void testJava2Elisp1() {
        assertEquals("jdee-foo-call-left-right", Symbol.java2Elisp("jdeeFooCallLeftRight"));
    }

    @Test
    public void testJava2Elisp2() {
        assertEquals("jdee-foo-call-left-right", Symbol.java2Elisp("jdee.foo.Call.leftRight"));
    }

    @Test
    public void testJava2Elisp3() {
        assertEquals("jdee-foo-call-left-right", Symbol.java2Elisp("jdee.foo.CALL_LEFT_RIGHT"));
    }
}
