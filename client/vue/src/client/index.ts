interface User {
    id: number,
    username: string,
    email: string,
}


class Client {
    constructor() { }
    #getToken() {
        return localStorage.getItem('token');
    }
    #setToken(token: string) {
        localStorage.setItem('token', token);
    }
    #deleteToken() {
        localStorage.removeItem('token');
    }
    #headers() {
        let token = this.#getToken();
        let headers = new Headers({});
        if (token) {
            console.log('HEADERS', token)
            console.log(`Bearer ${token}`)
            console.log(token);
            headers.append('Authorization', `Bearer ${token}`);
        }
        return headers;
    }
    async #get(url: string) {
        return fetch(url, {
            method: 'GET',
            headers: this.#headers(),
        });
    }
    async #delete(url: string) {
        return fetch(url, {
            method: 'DELETE',
            headers: this.#headers(),
        });
    }
    async #post(url: string, body?: any) {
        let headers = this.#headers();
        let request: RequestInit = { method: 'POST', headers };
        if (body) {
            request['body'] = JSON.stringify(body);
            headers.append('Content-Type', 'application/json');
        }
        return fetch(url, request);
    }
    async login(username: string, password: string) {
        let response = await this.#post('http://localhost:8000/login/', { username, password });
        if (response.status != 200) {
            throw 'Bad credentials';
        }
        let token = await response.json();
        this.#setToken(token);
    }
    async logout() {
        this.#deleteToken();
    }
    async register(username: string, email: string, password: string) {
        let response = await this.#post('http://localhost:8000/register/', { username, email, password });
        if (response.status != 201) {
            throw 'Couldnt register';
        }
        return await this.login(username, password);
    }
    async getCurrentUser(): Promise<User> {
        let response = await this.#get('http://localhost:8000/user/');
        if (response.status != 200) {
            this.#deleteToken();
            throw 'Not logged in';
        }
        let user = await response.json();
        return user;
    }
    async getBoards(): Promise<Array<any>> {
        let response = await this.#get('http://localhost:8000/games/');
        let json = await response.json();
        return json
    }
    async createNewBoard(size: number): Promise<number> {
        let response = await this.#post('http://localhost:8000/games/', { size });
        let json = await response.json();
        return json['id'];
    }
    async deleteBoard(id: number) {
        await this.#delete(`http://localhost:8000/games/${id}/`);
    }
    async getBoard(id: number) {
        let response = await this.#get(`http://localhost:8000/games/${id}/`);
        let json = await response.json();
        return json;
    }
    async playStone(id: number, x: number, y: number) {
        let response = await this.#post(`http://localhost:8000/games/${id}/play/`, { x, y });
        return await response.json();
    }
    async undo(id: number) {
        let response = await this.#post(`http://localhost:8000/games/${id}/undo/`);
        return await response.json();
    }
}

export default Client;
export type { User };