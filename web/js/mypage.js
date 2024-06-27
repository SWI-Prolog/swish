// AJAX 요청을 위한 함수 설정
function sendUpdateRequest(url, data) {
    fetch(url, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify(data)
    })
    .then(response => response.json())
    .then(data => {
        alert('Update successful!');
    })
    .catch((error) => {
        console.error('Error:', error);
        alert('An error occurred while updating.');
    });
}

// 사용자 이름 업데이트
function updateUsername() {
    const username = document.getElementById('username').value;
    sendUpdateRequest('/update_username', {username: username});
}

// 이메일 업데이트
function updateEmail() {
    const email = document.getElementById('email').value;
    sendUpdateRequest('/update_email', {email: email});
}

// 비밀번호 업데이트
function updatePassword() {
    const password = document.getElementById('password').value;
    sendUpdateRequest('/update_password', {password: password});
}

// 계정 삭제
function deleteAccount() {
    if (confirm("Are you sure you want to delete your account?")) {
        sendUpdateRequest('/delete_account', {})
        .then(() => {
            window.location.href = '/'; // 계정 삭제 후 메인 페이지로 리디렉션
        });
    }
}
