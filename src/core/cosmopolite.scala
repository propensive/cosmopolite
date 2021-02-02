package cosmopolite

import scala.reflect._
import scala.util.NotGiven
import scala.annotation._
import scala.quoted._

case class Language[+L <: String](value: String)

object Language:
   @targetName("make")
   def apply[L <: String: ValueOf]: Language[L] = new Language(summon[ValueOf[L]].value)
   
   inline def parse[L <: String](str: String): Option[Language[L]] = ${doParse[L]('str)}

   private def doParse[L <: String: Type](str: Expr[String])(using quotes: Quotes): Expr[Option[Language[L]]] =
      import quotes.reflect._

      def langs(t: TypeRepr): List[String] = t.dealias match
         case OrType(left, right) => langs(left) ++ langs(right)
         case ConstantType(StringConstant(lang)) => List(lang)

      langs(TypeRepr.of[L]).foldLeft('{ None: Option[Language[L]] }) { (agg, lang) =>
         '{ if $str == ${Expr(lang)} then Some(Language[L](${Expr(lang)})) else $agg }
      }

object I18n:
   def apply[L <: String: ValueOf](seq: Seq[String], parts: Seq[I18n[String, ? >: L]]): I18n[String, L] =
      val string = seq.head+(parts.zip(seq.tail).map { (msg, s) => msg(summon[ValueOf[L]])+s }.mkString)
      I18n[String, L](Map(summon[ValueOf[L]].value -> string))
   
case class I18n[T, -L <: String](text: Map[String, T]):
   def &[L2 <: String & Singleton](messages: I18n[T, L2])(using NotGiven[L2 <:< L]): I18n[T, L | L2] =
      I18n(text ++ messages.text)
   
   def apply[L2 <: L: ValueOf]: T = text(summon[ValueOf[L2]].value)
   def apply[L2 <: L]()(using ctx: Language[L2]): T = text(ctx.value)

import languages.common._

extension[L <: String] (str: String)
   def as(using ValueOf[L]): I18n[String, L] = I18n[L](List(str), Nil)

extension (ctx: StringContext)
   def en(msgs: I18n[String, En]*): I18n[String, En] = I18n(ctx.parts, msgs)
   def ru(msgs: I18n[String, Ru]*): I18n[String, Ru] = I18n(ctx.parts, msgs)
   def de(msgs: I18n[String, De]*): I18n[String, De] = I18n(ctx.parts, msgs)
   def es(msgs: I18n[String, Es]*): I18n[String, Es] = I18n(ctx.parts, msgs)
   def fr(msgs: I18n[String, Fr]*): I18n[String, Fr] = I18n(ctx.parts, msgs)
   def ja(msgs: I18n[String, Ja]*): I18n[String, Ja] = I18n(ctx.parts, msgs)
   def pt(msgs: I18n[String, Pt]*): I18n[String, Pt] = I18n(ctx.parts, msgs)
   def zh(msgs: I18n[String, Zh]*): I18n[String, Zh] = I18n(ctx.parts, msgs)
   def it(msgs: I18n[String, It]*): I18n[String, It] = I18n(ctx.parts, msgs)
   def pl(msgs: I18n[String, Pl]*): I18n[String, Pl] = I18n(ctx.parts, msgs)

