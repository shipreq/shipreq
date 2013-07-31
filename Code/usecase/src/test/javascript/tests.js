var liftAjax = {
    lift_ajaxHandler: function(){return false}
}

var ids = {
    title: "uc-title"
    ,tf0: "F808585046428RZEI0V"
    ,tf1: "F808585046429ODSPMQ"
    ,tf2: "F808585046430I4NI1A"
    ,s_1_0: "s1049-t"
    ,s_1_0_1: "s1046-t"
    ,s_1_0_2: "s1048-t"
    ,s_1_0_2_a: "s1047-t"
    ,s_1_0_3: "s1028-t"
    ,s_1_1: "s1050-t"
    ,s_1_e_1: "s1043-t"
    ,s_1_e_2: "s1044-t"
    ,tf3: "F8085850464312XOWYC"
    ,tf8: "F8085850464365THSLV"
}

function $id(id) {
    return $("#" + id)
}

function testBelow(desc, from, to) {
    test("Finds textarea below ("+desc+")", function () { equal(getElementBelow(from).id, to) });
}
function testAbove(desc, from, to) {
    test("Finds textarea above ("+desc+")", function () { equal(getElementAbove(from).id, to) });
}

testAbove("title -> bottom", ids.title, ids.tf8)
testAbove("text -> title", ids.tf0, ids.title)
testAbove("text -> text", ids.tf2, ids.tf1)
testAbove("NC -> text", ids.s_1_0, ids.tf2)
testAbove("NC -> NC", ids.s_1_0_3, ids.s_1_0_2_a)
testAbove("AC -> NC", ids.s_1_1, ids.s_1_0_3)
testAbove("EC -> AC", ids.s_1_e_1, ids.s_1_1)
testAbove("EC -> EC", ids.s_1_e_2, ids.s_1_e_1)
testAbove("text -> EC", ids.tf3, ids.s_1_e_2)

testBelow("bottom -> title", ids.tf8, ids.title)
testBelow("title -> text", ids.title, ids.tf0)
testBelow("text -> text", ids.tf1, ids.tf2)
testBelow("text -> NC", ids.tf2, ids.s_1_0)
testBelow("NC -> NC", ids.s_1_0_2_a, ids.s_1_0_3)
testBelow("NC -> AC", ids.s_1_0_3, ids.s_1_1)
testBelow("AC -> EC", ids.s_1_1, ids.s_1_e_1)
testBelow("EC -> EC", ids.s_1_e_1, ids.s_1_e_2)
testBelow("EC -> text", ids.s_1_e_2, ids.tf3)

function assertLosesFocus(elementId, fn) {
    var e = $id(elementId)
    e.focus()
    equal( e.is(':focus'), true, "Initial focus failed." )
    fn()
    equal( e.is(':focus'), false, "Focus didn't change." )
}
function testFocusChange(name, fn, from, to) {
    test("Keyboard shortcut: " + name, function(){
        assertLosesFocus(from, fn)
        equal( $id(to).is(':focus'), true, "Target didn't get focus." )
    })
}
testFocusChange("Alt + Down", onAltDown, ids.s_1_0, ids.s_1_0_1)
testFocusChange("Alt + Up",   onAltUp,   ids.s_1_0, ids.tf2)

test("Keyboard shortcut: Esc", function(){
    assertLosesFocus(ids.s_1_0, onEscape)
    equal( $(':focus').length, 0, "Nothing should have focus." )
})
