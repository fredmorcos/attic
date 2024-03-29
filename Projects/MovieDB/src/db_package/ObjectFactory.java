//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, vJAXB 2.0 in JDK 1.6 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.06.03 at 11:04:23 PM EEST 
//


package db_package;

import java.math.BigInteger;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the db_package package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _Actriss_QNAME = new QName("", "actriss");
    private final static QName _MajorActors_QNAME = new QName("", "major_actors");
    private final static QName _FName_QNAME = new QName("", "f_name");
    private final static QName _Movie_QNAME = new QName("", "movie");
    private final static QName _MovieDatabase_QNAME = new QName("", "Movie_database");
    private final static QName _Actor_QNAME = new QName("", "actor");
    private final static QName _Director_QNAME = new QName("", "director");
    private final static QName _Country_QNAME = new QName("", "country");
    private final static QName _Title_QNAME = new QName("", "title");
    private final static QName _Adress_QNAME = new QName("", "adress");
    private final static QName _LName_QNAME = new QName("", "l_name");
    private final static QName _Email_QNAME = new QName("", "email");
    private final static QName _Name_QNAME = new QName("", "name");
    private final static QName _ProductionCompany_QNAME = new QName("", "production_company");
    private final static QName _Year_QNAME = new QName("", "year");
    private final static QName _Language_QNAME = new QName("", "language");
    private final static QName _User_QNAME = new QName("", "user");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: db_package
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link MovieDb }
     * 
     */
    public MovieDb createMovieDb() {
        return new MovieDb();
    }

    /**
     * Create an instance of {@link MajorActorsType }
     * 
     */
    public MajorActorsType createMajorActorsType() {
        return new MajorActorsType();
    }

    /**
     * Create an instance of {@link UserType }
     * 
     */
    public UserType createUserType() {
        return new UserType();
    }

    /**
     * Create an instance of {@link NameType }
     * 
     */
    public NameType createNameType() {
        return new NameType();
    }

    /**
     * Create an instance of {@link MovieType }
     * 
     */
    public MovieType createMovieType() {
        return new MovieType();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "actriss")
    public JAXBElement<String> createActriss(String value) {
        return new JAXBElement<String>(_Actriss_QNAME, String.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MajorActorsType }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "major_actors")
    public JAXBElement<MajorActorsType> createMajorActors(MajorActorsType value) {
        return new JAXBElement<MajorActorsType>(_MajorActors_QNAME, MajorActorsType.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "f_name")
    public JAXBElement<String> createFName(String value) {
        return new JAXBElement<String>(_FName_QNAME, String.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MovieType }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "movie")
    public JAXBElement<MovieType> createMovie(MovieType value) {
        return new JAXBElement<MovieType>(_Movie_QNAME, MovieType.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MovieDb }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "Movie_database")
    public JAXBElement<MovieDb> createMovieDatabase(MovieDb value) {
        return new JAXBElement<MovieDb>(_MovieDatabase_QNAME, MovieDb.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "actor")
    public JAXBElement<String> createActor(String value) {
        return new JAXBElement<String>(_Actor_QNAME, String.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "director")
    public JAXBElement<String> createDirector(String value) {
        return new JAXBElement<String>(_Director_QNAME, String.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "country")
    public JAXBElement<String> createCountry(String value) {
        return new JAXBElement<String>(_Country_QNAME, String.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "title")
    public JAXBElement<String> createTitle(String value) {
        return new JAXBElement<String>(_Title_QNAME, String.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "adress")
    public JAXBElement<String> createAdress(String value) {
        return new JAXBElement<String>(_Adress_QNAME, String.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "l_name")
    public JAXBElement<String> createLName(String value) {
        return new JAXBElement<String>(_LName_QNAME, String.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "email")
    public JAXBElement<String> createEmail(String value) {
        return new JAXBElement<String>(_Email_QNAME, String.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link NameType }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "name")
    public JAXBElement<NameType> createName(NameType value) {
        return new JAXBElement<NameType>(_Name_QNAME, NameType.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "production_company")
    public JAXBElement<String> createProductionCompany(String value) {
        return new JAXBElement<String>(_ProductionCompany_QNAME, String.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BigInteger }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "year")
    public JAXBElement<BigInteger> createYear(BigInteger value) {
        return new JAXBElement<BigInteger>(_Year_QNAME, BigInteger.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link String }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "language")
    public JAXBElement<String> createLanguage(String value) {
        return new JAXBElement<String>(_Language_QNAME, String.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link UserType }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "user")
    public JAXBElement<UserType> createUser(UserType value) {
        return new JAXBElement<UserType>(_User_QNAME, UserType.class, null, value);
    }

}

