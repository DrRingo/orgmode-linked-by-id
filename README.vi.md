# Org-mode Linking by ID

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Emacs](https://img.shields.io/badge/Emacs-26.1+-blue.svg)](https://www.gnu.org/software/emacs/)
[![Org-mode](https://img.shields.io/badge/Org--mode-9.0+-green.svg)](https://orgmode.org/)

Extension cho Emacs Org-mode để tạo liên kết chéo giữa các header bằng cách sử dụng ID thay vì nội dung header.

## 🌟 Tính năng

### 1. Liên kết Header trong File hiện tại
- Tìm tất cả headers có ID trong file đang mở
- Cho phép chọn header từ danh sách
- Chèn liên kết ID vào vị trí con trỏ

### 2. Liên kết Header trong Thư mục
- Tìm headers có ID trong tất cả file .org trong thư mục (không bao gồm thư mục con)
- Sử dụng Helm với fuzzy search để tìm kiếm dễ dàng
- Chọn thư mục tương tác

### 3. Liên kết đến Hình ảnh/Bảng có tên
- Tìm các đối tượng có `#+NAME` và `#+CAPTION`
- Hiển thị danh sách caption cho người dùng chọn
- Cho phép tùy chỉnh description của link

## 🚀 Cài đặt

> **Cài đặt nhanh**: Xem [INSTALL.md](INSTALL.md) để cài đặt nhanh nhất.

### Yêu cầu hệ thống

- Emacs 26.1 trở lên
- Org-mode
- Helm (cho fuzzy search và tìm kiếm đối tượng)

### Cách cài đặt

#### Phương pháp 1: Sử dụng use-package (Khuyến nghị)

Thêm vào file config của bạn (`.emacs`, `init.el`, hoặc `config.el`):

```elisp
(use-package orgmode-linked-by-id
  :straight (:type git :host github :repo "drringo/orgmode-linked-by-id")
  :config
  (require 'orgmode-linked-by-id))
```

#### Phương pháp 2: Sử dụng straight.el

```elisp
(straight-use-package
 '(orgmode-linked-by-id :type git :host github :repo "drringo/orgmode-linked-by-id"))
```

#### Phương pháp 3: Cài đặt thủ công

```bash
git clone https://github.com/drringo/orgmode-linked-by-id.git ~/.emacs.d/orgmode-linked-by-id
```

Thêm vào file config:
```elisp
(add-to-list 'load-path "~/.emacs.d/orgmode-linked-by-id")
(require 'orgmode-linked-by-id)
```

## ⌨️ Phím tắt

| Phím tắt | Chức năng | Mô tả |
|----------|-----------|-------|
| `C-c l i` hoặc `SPC l i` | Chèn ID link trong file hiện tại | Tìm headers trong file đang mở |
| `C-c l f` hoặc `SPC l f` | Chèn ID link trong thư mục | Tìm headers trong thư mục |
| `C-c l c` hoặc `SPC l c` | Chèn link đến hình ảnh/bảng có tên | Tìm đối tượng có NAME/CAPTION |

## 📖 Hướng dẫn sử dụng

### Quy trình làm việc cơ bản

1. **Tạo ID cho header**: `SPC m I`
2. **Tạo liên kết đến header khác**:
   - `C-c l i`: Tìm trong file hiện tại
   - `C-c l f`: Tìm trong thư mục
   - `C-c l c`: Tìm hình ảnh/bảng có tên
3. Chọn header từ danh sách
4. Tùy chỉnh description nếu cần
5. Link được chèn vào vị trí con trỏ

### Ví dụ sử dụng

```org
* Header 1
:PROPERTIES:
:ID: abc123
:END:

* Header 2
:PROPERTIES:
:ID: def456
:END:

Nội dung tham chiếu đến [[id:abc123][Header 1]] và [[id:def456][Header 2]].

#+NAME: my-image
#+CAPTION: Hình ảnh mẫu
[[file:image.png]]

Liên kết đến [[my-image][Hình ảnh mẫu]].
```

## 🎯 Lý do sử dụng ID

- **Tránh vấn đề khi header thay đổi nội dung**
- **Không cần suy nghĩ về CUSTOM_ID**
- **ID được tạo tự động bằng `org-id-get-create`**

## 📊 So sánh với phương pháp truyền thống

| Phương pháp truyền thống | Extension này |
|--------------------------|---------------|
| `[[*Header name][Header name]]` | `[[id:abc123][Header name]]` |
| Hỏng khi header thay đổi | Vẫn hoạt động |
| Khó tìm kiếm | Tìm kiếm dễ dàng |

## 🤝 Đóng góp

Mọi đóng góp đều được chào đón! Vui lòng:

1. Fork repository
2. Tạo branch mới (`git checkout -b feature/amazing-feature`)
3. Commit thay đổi (`git commit -m 'Add amazing feature'`)
4. Push lên branch (`git push origin feature/amazing-feature`)
5. Tạo Pull Request

## 📝 License

Dự án này được phân phối dưới giấy phép MIT. Xem file [LICENSE](LICENSE) để biết thêm chi tiết.

## 🔗 Liên kết

- **Repository**: https://github.com/drringo/orgmode-linked-by-id
- **Issues**: https://github.com/drringo/orgmode-linked-by-id/issues
- **Discussions**: https://github.com/drringo/orgmode-linked-by-id/discussions
- **Troubleshooting**: [TROUBLESHOOTING.md](TROUBLESHOOTING.md)

## 📚 Tài liệu thêm

Xem file [intro.md](intro.md) để đọc bài blog chi tiết về dự án này.

---

⭐ Nếu dự án này hữu ích, hãy cho một star! 