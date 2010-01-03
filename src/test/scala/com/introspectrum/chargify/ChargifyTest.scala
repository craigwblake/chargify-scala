package com.introspectrum.chargify

import java.util.Properties
import junit.framework._

class ChargifyTest extends TestCase {

	val properties = new Properties()
	try {
		properties.load( getClass().getClassLoader().getResourceAsStream( "test.properties"))
	} catch {
		case e => throw new RuntimeException( "Please create 'src/test/resources/test.properties' file with 'key' and 'domain' properties.")
	}

	def testList() = {
		val chargify = new Chargify( properties.getProperty( "key"), properties.getProperty( "domain"))
		val customers = chargify.listCustomers()
		customers.foreach( println)
	}
}
