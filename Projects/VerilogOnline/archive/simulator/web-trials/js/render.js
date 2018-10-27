window.onload = function() {
    var paper = Raphael(document.getElementById("machine_div"),
			machine.width, machine.height);
    paper.clear();
    paper.add(machine.objects);
}
