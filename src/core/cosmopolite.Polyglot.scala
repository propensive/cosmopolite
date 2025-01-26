/*
    Cosmopolite, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cosmopolite

import proscenium.*

case class Polyglot[+ValueType, LanguageType](values: Map[Language, ValueType]):
  @targetName("or")
  transparent inline infix def | [ValueType2 >: ValueType, LanguageType2]
    (polyglot: Polyglot[ValueType2, LanguageType2])
  :     Polyglot[ValueType2, LanguageType & LanguageType2] | ValueType2 =
    compiletime.summonFrom:
      case locale: Locale[LanguageType & LanguageType2] =>
        (values ++ polyglot.values)(locale.language)

      case _ =>
        Polyglot[ValueType2, LanguageType & LanguageType2](values ++ polyglot.values)

  def apply()(using locale: Locale[LanguageType]): ValueType = values(locale.language)
