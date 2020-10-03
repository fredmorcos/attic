window.addEventListener('DOMContentLoaded', function(event) {
    console.log("host location = " + location.host);
    var uri = 'ws://' + location.host + "/ws";
    var ws;

    go.onclick = function() {
        console.log("Sending a message...");
        ws = new WebSocket(uri);

        ws.onmessage = function(msg) {
            // message(msg.data);
            console.log("got message = ", msg);
        };

        ws.onclose = function() {
            console.log("socket closed!");
        }

        ws.onopen = function() {
            console.log("Websocket connected");
            gender = undefined;

            if (male.checked) {
                gender = "Male";
            } else if (female.checked) {
                gender = "Female";
            } else if (transmale.checked) {
                gender = "TransMale";
            } else if (transfemale.checked) {
                gender = "TransFemale";
            }

            search_genders = [];

            if (search_male.checked) {
                search_genders.push("Male");
            }

            if (search_female.checked) {
                search_genders.push("Female");
            }

            if (search_transmale.checked) {
                search_genders.push("TransMale");
            }

            if (search_transfemale.checked) {
                search_genders.push("TransFemale");
            }

            query = {
                "gender": gender,
                "age": age.value,
                "search_genders": search_genders,
                "search_age": search_age.value,
                "search_sexual": search_sexual.checked,
            };

            ws.send(JSON.stringify(query));
        }
    }
});

// function message(data) {
//     var line = document.createElement('p');
//     line.innerText = data;
//     chat.appendChild(line);
// }

// send.onclick = function() {
//     var msg = text.value;
//     ws.send(msg);
//     text.value = '';
//     message('<You>: ' + msg);
// };
