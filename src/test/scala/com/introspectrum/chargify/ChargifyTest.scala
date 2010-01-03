package com.introspectrum.chargify

import junit.framework._

class ChargifyTest extends TestCase {

	def testList() = {
		val chargify = new Chargify( System.getProperty( "key"), System.getProperty( "domain"))
		val customers = chargify.listCustomers()
		customers.foreach( println)
	}
}
